import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

let knownCallingConvs = ["HsCallJSON"]

public struct ForeignImportHaskellMacro: PeerMacro {
    public static func expansion(of node: AttributeSyntax, providingPeersOf declaration: some DeclSyntaxProtocol, in context: some MacroExpansionContext) throws -> [DeclSyntax] {
        guard let function = declaration.as(FunctionDeclSyntax.self) else {
            throw ForeignImportHaskellError.onlyApplicableToFunction
        }
        
        let sig = function.signature,
            fname = function.name
        
        // If I wanted the calling convention used I could use a "guard let" instead of "!= nil"
        guard knownCallingConvs.firstIndex(of: sig.parameterClause.parameters.first?.type.description ?? "") != nil else {
            throw ForeignImportHaskellError.unknownCallingConv
        }
        
        guard let retType:TypeSyntax = sig.returnClause?.type.trimmed else {
            throw ForeignImportHaskellError.noReturnClause
        }
        
        let usefulArgs = sig.parameterClause.parameters.dropFirst()
        
        let args = usefulArgs.map({ param in
            "\(param.firstName)\(param.secondName ?? ""): \(param.type.trimmed)" }).joined(separator: ", "),
            
            argNames = usefulArgs.map({ param in
                let name = param.secondName ?? param.firstName
                return (name.text.filter({ $0 != " " }), param.type.trimmed)
            }),
            
            encodedArgs = argNames.map({(arg, arg_ty) in
                if arg_ty.description == "UnsafeRawPointer" {
                    // We receive stable ptrs unchanged
                    return ""
                }
                else {
                    return """
                    var \(arg)_data = try hs_enc.encode(\(arg))
                    let \(arg)_datalen = Int(\(arg)_data.count)
                    """
                }
            }).joined(separator: "\n"),
        
            pointersToArgs = argNames.map({(arg, arg_ty) in
                if arg_ty.description == "UnsafeRawPointer" {
                    return ""
                }
                else {
                    return """
                    try \(arg)_data.withUnsafeMutableBytes { (\(arg)_ptr:UnsafeMutableRawBufferPointer) in
                    """
                }
            }).joined(separator: "\n"),
            
            closePointersToArgsClosures = argNames.map({(_,arg_ty) in
                if arg_ty.description == "UnsafeRawPointer" {
                    return ""
                }
                else {
                    return "}"
                }
            }).joined(separator: "\n"),
        
            fcallArgs = (argNames.map({(arg, arg_ty) in
                if arg_ty.description == "UnsafeRawPointer" {
                    "\(arg)"
                }
                else {
                    "\(arg)_ptr.baseAddress, \(arg)_datalen"
                }
            }) + {
                // If the result is serialized then we need two extra arguments
                if retType.description == "UnsafeRawPointer" {
                    []
                }
                else {
                    ["res_ptr.baseAddress", "size_ptr.baseAddress"]
                }
            }()).joined(separator: ", "),
        
            fcall:ExprSyntax =
                "h\(fname)(\(raw: fcallArgs))",
            
            decodeAndReturnResult:StmtSyntax =
                """
                let new_data = Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none)
                print("Read JSON from Haskell: \\(String(bytes: new_data, encoding: .utf8) ?? "???")")
                return try hs_dec.decode(\(retType).self, from: new_data)
                """,
        
            callAndReturn:StmtSyntax = {
                if retType.description == "UnsafeRawPointer" {
                    return "return \(fcall)"
                }
                else {
                    return """
                    // Allocate buffer for result and allocate a pointer to an int with the initial size of the buffer
                    let buf_size = 1024000
                    enum HsFFIError: Error {
                        case requiredSizeIs(Int)
                    }
                    return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
                        size_ptr.baseAddress?.pointee = buf_size
                        
                        do {
                            return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) { res_ptr in
                                
                                \(fcall)
                                
                                if let required_size = size_ptr.baseAddress?.pointee {
                                    if required_size > buf_size {
                                        throw HsFFIError.requiredSizeIs(required_size)
                                    }
                                }
                    
                                \(decodeAndReturnResult)
                            }
                        } catch HsFFIError.requiredSizeIs(let required_size) {
                            print("Retrying with required size: \\(required_size)")
                            return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment: 1) { res_ptr in
                                size_ptr.baseAddress?.pointee = required_size
                                
                                \(fcall)
                                \(decodeAndReturnResult)
                            }
                        }
                    }
                    """
                }
            }()
        
        
        return [
        """
        func \(fname)(\(raw: args)) -> \(retType) {
          \(raw: args.isEmpty ? "" : "let hs_enc = JSONEncoder()")
          let hs_dec = JSONDecoder()
          do {
            \(raw: encodedArgs)
            return try {
              \(raw: pointersToArgs)
                \(callAndReturn)
              \(raw: closePointersToArgsClosures)
            }()
          } catch {
              fatalError("Error decoding JSON marshaled from Haskell, probably: \\(error)")
          }
        }
        """
        ]
    }
}

public enum ForeignImportHaskellError: CustomStringConvertible, Error {
    case onlyApplicableToFunction
    case unknownCallingConv
    case noReturnClause

    public var description: String {
        switch self {
        case .onlyApplicableToFunction:
            "@ForeignImportHaskell can only be applied to a function."
        case .unknownCallingConv:
            "@ForeignImportHaskell can only be applied to functions whose first argument is a stub parameter whose type indicates the calling convention. Known types include: \(knownCallingConvs.joined(separator: ", "))."
        case .noReturnClause:
            "@ForeignImportHaskell can only be applied to functions with a return clause."
        }
    }
}

@main
struct ForeignImportHaskellMacroPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        ForeignImportHaskellMacro.self
    ]
}

