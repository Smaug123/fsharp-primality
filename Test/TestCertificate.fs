namespace Primality.Test

open FsCheck
open FsUnitTyped
open NUnit.Framework

open Primality

module TestCertificate =

    [<TestCase 561>]
    [<TestCase 1105>]
    [<TestCase 1729>]
    [<TestCase 2465>]
    [<TestCase 2821>]
    [<TestCase 6601>]
    [<TestCase 8911>]
    [<TestCase 10585>]
    [<TestCase 15841>]
    [<TestCase 29341>]
    [<TestCase 41041>]
    [<TestCase 46657>]
    [<TestCase 52633>]
    [<TestCase 62745>]
    let ``Test a Carmichael number`` (i : int) =
        match Certificate.find i with
        | Certificate.Composite (Fermat n) ->
            i % n |> shouldEqual 0
        | _ -> failwith "Unexpected!"