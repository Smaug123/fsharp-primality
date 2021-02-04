namespace Primality.Test

open FsUnitTyped
open NUnit.Framework

open Primality

module TestArithmetic =

    [<TestCase (2, 3, 5, 3)>]
    let ``Test powerMod`` (baseNum : int, power : int, modulus : int, expected : int) =
        Arithmetic.powerMod baseNum power modulus
        |> shouldEqual expected