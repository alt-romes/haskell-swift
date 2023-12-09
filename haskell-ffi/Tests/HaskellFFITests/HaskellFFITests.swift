import XCTest
@testable import HaskellFFI
import Foundation

struct User: Codable {
    let name: String
    let age: Int
}

@ForeignImportHaskell
func birthday(cconv: HsCallJSON, user: User) -> User { hstub() }

@ForeignImportHaskell
func f1(cconv: HsCallJSON, user: User, p: UnsafeMutableRawPointer) -> User { hstub() }

@ForeignImportHaskell
func f2(cconv: HsCallJSON, user: User) -> UnsafeMutableRawPointer { hstub() }

@ForeignImportHaskell
func f3(cconv: HsCallJSON, user: User, p: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer { hstub() }

final class HaskellFFITests: XCTestCase {
    func testExample() throws {
        // XCTest Documentation
        // https://developer.apple.com/documentation/xctest

        // Defining Test Cases and Test Methods
        // https://developer.apple.com/documentation/xctest/defining_test_cases_and_test_methods
    }
}

/* Test Stubs
 (We only really care about type checking the generated code)
 */
func hbirthday(_ a: UnsafeMutableRawPointer?, _ asize: Int64, _ b: UnsafeMutableRawPointer?, _ bsize: UnsafeMutablePointer<Int>?) { hstub() }
func hf1(_ a: UnsafeMutableRawPointer?, _ asize: Int64, _ b: UnsafeMutableRawPointer, _ c: UnsafeMutableRawPointer?, _ csize: UnsafeMutablePointer<Int>?) { hstub() }
func hf2(_ a: UnsafeMutableRawPointer?, _ asize: Int64) -> UnsafeMutableRawPointer { hstub() }
func hf3(_ a: UnsafeMutableRawPointer?, _ asize: Int64, _ b: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer { hstub() }

