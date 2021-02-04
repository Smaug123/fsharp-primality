namespace Primality.Test

open FsCheck
open FsUnitTyped
open NUnit.Framework

open Primality

module TestArithmetic =

    [<TestCase (2, 3, 5, 3)>]
    [<TestCase (100, 31, 309, 61)>]
    let ``Test powerMod`` (baseNum : int, power : int, modulus : int, expected : int) =
        Arithmetic.powerMod modulus baseNum power
        |> shouldEqual expected

    [<Test>]
    let ``powerMod with zero modulus`` () =
        let exn = Assert.Throws<System.DivideByZeroException> (fun () -> Arithmetic.powerMod 0 2 3 |> ignore)
        exn.Message |> shouldContainText "ttempted to divide by zero"

    [<Test>]
    let ``powerMod with negative modulus`` () =
        let exn = Assert.Throws<System.ArgumentOutOfRangeException> (fun () -> Arithmetic.powerMod (-1) 2 3 |> ignore)
        exn.Message |> shouldContainText "out of the range of valid values"
        exn.Message |> shouldContainText "'modulus'"

    [<Test>]
    let ``powerMod with negative power`` () =
        let exn = Assert.Throws<System.ArgumentOutOfRangeException> (fun () -> Arithmetic.powerMod 7 2 (-1) |> ignore)
        exn.Message |> shouldContainText "out of the range of valid values"
        exn.Message |> shouldContainText "'power'"

    [<Test>]
    let ``Property-based test: powerMod with zero modulus`` () =
        let property (baseNum : int) (power : int) : bool =
            let exn = Assert.Throws<System.DivideByZeroException> (fun () -> Arithmetic.powerMod 0 baseNum power |> ignore)
            exn.Message.Contains "ttempted to divide by zero"

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Property-based test: powerMod with negative modulus`` () =
        let property (baseNum : int) (power : int) (modulus : int) : bool =
            let modulus = -(abs modulus + 1)
            let exn = Assert.Throws<System.ArgumentOutOfRangeException> (fun () -> Arithmetic.powerMod modulus baseNum power |> ignore)
            exn.Message.Contains "out of the range of valid values" && exn.Message.Contains "'modulus'"

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Property-based test: powerMod with negative power`` () =
        let property (baseNum : int) (power : int) (modulus : int) : bool =
            let modulus = (abs modulus + 1)
            let power = -(abs power + 1)
            let exn = Assert.Throws<System.ArgumentOutOfRangeException> (fun () -> Arithmetic.powerMod modulus baseNum power |> ignore)
            exn.Message.Contains "out of the range of valid values" && exn.Message.Contains "'power'"

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Property-based sanity test of powerMod`` () =
        let property (baseNum : int) (power : int) (modulus : int) : bool =
            // Refine modulus to be nonzero
            let modulus = abs modulus + 1
            let power = abs power
            let actual = Arithmetic.powerMod modulus baseNum power
            actual < modulus && 0 <= actual

        Check.QuickThrowOnFailure property

    let ``powerMod with base zero is zero`` () =
        let property (power : int) (modulus : int) : bool =
            let power = abs power
            let modulus = abs modulus + 1
            let actual = Arithmetic.powerMod modulus 0 power
            actual = 0

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``powerMod with modulus 1 is always 0`` () =
        let property (baseNum : int) (power : int) : bool =
            let power = abs power
            let actual = Arithmetic.powerMod 1 baseNum power
            actual = 0

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Factor produces factors`` () =
        let property (i : int) : bool =
            Arithmetic.factor (abs i + 1)
            |> List.fold ((*)) 1
            |> (=) (abs i + 1)

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Factor is in descending order and contains no 1's`` () =
        let property (i : int) : bool =
            let fact = Arithmetic.factor (abs i + 1)
            fact |> List.sortDescending |> (=) fact
            && fact |> List.forall (fun i -> i > 1)

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Factor throws for nonpositive numbers`` () =
        let property (i : int) : bool =
            let exn = Assert.Throws<System.ArgumentOutOfRangeException> (fun () -> Arithmetic.factor (-abs i) |> ignore)
            exn.Message.Contains "'i'"

        Check.QuickThrowOnFailure property

[<TestFixture>]
[<Category "Exhaustive">]
[<Explicit "These tests are exhaustive tests of functions and may take a long time.">]
module TestArithmeticExhaustively =

    [<Test>]
    let ``Test addModulus`` () =
        for i in 0..127 do
            for j in 0..127 do
                for modulus in 1..127 do
                    let result =
                        Arithmetic.addMod (sbyte modulus) (sbyte i) (sbyte j)
                        |> int
                    if result <> (i + j) % modulus then
                        failwithf "oh no: %+A %+A %+A, got %A" i j modulus result
                    result
                    |> shouldEqual ((i + j) % modulus)

    [<Test>]
    let ``Test timesMod`` () =
        for i in 0..127 do
            for j in 0..127 do
                for modulus in 1..127 do
                    let result =
                        Arithmetic.timesMod (sbyte modulus) (sbyte i) (sbyte j)
                        |> int
                    if result <> ((i * j) % modulus) then failwithf "oh no: %+A * %+A (mod %+A), got %+A" i j modulus result
                    result
                    |> shouldEqual ((i * j) % modulus)

    [<Test>]
    let ``Test powerMod at the boundaries`` () =
        for i in 0..127 do
            for j in 0..127 do
                for modulus in 1..127 do
                    let result =
                        Arithmetic.powerMod (sbyte modulus) (sbyte i) (sbyte j)
                        |> int
                    let biggerResult = Arithmetic.powerMod modulus i j
                    if result <> biggerResult then
                        failwithf "oh no: %+A %+A %+A, got %A" i j modulus result
