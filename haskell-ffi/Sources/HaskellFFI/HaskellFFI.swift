// The Swift Programming Language
// https://docs.swift.org/swift-book

/// A macro that produces a function <name> to wrap a call to a foreign function from
/// Haskell that has been exported as $(foreignExportSwift <name>) from the foreign library.
/// The type of the function must match the Haskell type in that each argument
/// and return type must be JSON-isomorphic. And additional "zero-eth" argument
/// with the calling convention must be added to the function, and a stub
/// implementation provided.
/// For example, the following Haskell function
///
///     birthday :: User -> User
///     birthday User{age=x, birthYear=y} = User{age=x+1, birthYear=y}
///     $(foreignExportSwift 'birthday)
///
/// can be imported into Swift by writing
///
///     func birthday(cconv: HsCallJSON, user: User) -> User { stub() }
///
/// and then simply called as
///
///     birthday(user: User(23, 2023))
///
@attached(peer, names: overloaded)
public macro ForeignImportHaskell() = #externalMacro(module: "HaskellFFIMacros", type: "ForeignImportHaskellMacro")

public struct HsCallJSON {
    init() {
        fatalError("HsCallJSON should never be constructed, it is just used in the stub function signature from which we generate the actual function that calls a Haskell function.")
    }
}

func stub() -> Never {
    fatalError("Somehow, a stub for a foreign-imported Haskell function was called")
}

