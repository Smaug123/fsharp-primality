namespace TestPrimality

open FsCheck
open FsUnitTyped
open NUnit.Framework

open Primality

module TestPrimality =

    [<Test>]
    let ``Primality of 3`` () =
        Certificate.find 3
        |> shouldEqual (Certificate.Prime (PrimeCertificate.Pratt { Base = 2 ; Factors = [PrimeCertificate.Two] ; Modulus = 3 }))

    // Example drawn from Wikipedia
    [<Test>]
    let ``Primality of 229`` () =
        let three =
            PrimeCertificate.Pratt
                {
                    Modulus = 3
                    Base = 2
                    Factors = [ PrimeCertificate.Two ]
                }
        let expected =
            {
                Base = 6
                Factors =
                    [
                        PrimeCertificate.Pratt
                            {
                                Modulus = 19
                                Base = 2
                                Factors = [ three ; three ; PrimeCertificate.Two ]
                            }
                        three
                        PrimeCertificate.Two
                        PrimeCertificate.Two
                    ]
                Modulus = 229
            }

        Certificate.find 229
        |> shouldEqual (Certificate.Prime (PrimeCertificate.Pratt expected))

    [<Test>]
    let ``Hooray`` () =
        for i in 0..10000 do
            let cert = Certificate.find i
            match cert with
            | Prime cert ->
                PrimeCertificate.toInt cert |> shouldEqual i
                PrimeCertificate.verify cert |> shouldEqual true
            | Composite cert ->
                match cert with
                | CompositeCertificate.Factor fact ->
                    i % fact |> shouldEqual 0
                    fact |> shouldBeGreaterThan 1
                    fact |> shouldBeSmallerThan i
                | CompositeCertificate.Fermat _
                | CompositeCertificate.SquareRoot _ ->
                    Arithmetic.factor i
                    |> List.length
                    |> shouldBeGreaterThan 1
            | One -> i |> shouldEqual 1
            | Zero -> i |> shouldEqual 0