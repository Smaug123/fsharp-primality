namespace Primality

open Primality.Internals

type PrattError =
    /// We found a nontrivial factor of the input integer.
    | FoundFactor of int
    /// The input integer was less than 2.
    | TooSmall
    /// The input integer was even.
    | IsEven
    /// We were unable to find a Pratt certificate; the number is composite, but we didn't find a factor.
    | Failed

[<RequireQualifiedAccess>]
module PrimeCertificate =

    let toInt (c : PrimeCertificate<int>) : int =
        match c with
        | PrimeCertificate.Two -> 2
        | PrimeCertificate.Pratt p ->
            p.Modulus

    let private findPrattSkeleton (i : int) : Result<int * int list, PrattError>=
        if i % 2 = 0 then Error PrattError.IsEven else
        if i < 2 then Error PrattError.TooSmall else
        let factors = Arithmetic.factor (i - 1)
        let distinctFactors = factors |> List.distinct
        // Find a base `a`, coprime to i, such that
        //   Arithmetic.powerMod i a (i - 1) = 1
        //   for each q in factors, Arithmetic.powerMod i a ((i-1)/q) <> 1.
        let rec go (possibleBase : int) =
            if possibleBase >= i then Error PrattError.Failed else
            let euclid = Arithmetic.euclideanAlgorithm possibleBase i
            if euclid.Hcf <> 1 then Error (PrattError.FoundFactor euclid.Hcf) else
            if Arithmetic.powerMod i possibleBase (i - 1) = 1 then
                if List.forall (fun q -> Arithmetic.powerMod i possibleBase ((i - 1) / q) <> 1) distinctFactors then
                    Ok (possibleBase, factors)
                else go (possibleBase + 1)
            else go (possibleBase + 1)

        go 2

    let rec make'
        (known : Map<int, PrattCertificate<int>>)
        (i : int)
        : Map<int, PrattCertificate<int>> * Result<PrimeCertificate<int>, PrattError>
        =
        if i = 2 then known, Ok (PrimeCertificate.Two) else
        match Map.tryFind i known with
        | Some cert -> known, Ok (PrimeCertificate.Pratt cert)
        | None ->

        let skeleton = findPrattSkeleton i
        match skeleton with
        | Error e -> known, Error e
        | Ok (baseNum, factors) ->
            let known, distinct =
                factors
                |> List.distinct
                |> List.fold
                    (fun (known, acc) i ->
                        let known, result = make' known i
                        known, (i, Result.get result) :: acc
                    )
                    (known, [])
            let distinct = Map.ofList distinct
            let cert =
                {
                    Base = baseNum
                    Modulus = i
                    Factors =
                        factors |> List.map (fun i -> Map.find i distinct)
                }

            Map.add i cert known, Ok (PrimeCertificate.Pratt cert)

    let make (i : int) : Result<PrimeCertificate<int>, PrattError> =
        let _, result = make' Map.empty i
        result

    let rec verify (cert : PrimeCertificate<int>) : bool =
        match cert with
        | PrimeCertificate.Two -> true
        | PrimeCertificate.Pratt cert ->
            let factorsArePrime = cert.Factors |> List.forall verify
            let apparentFactorisation = cert.Factors |> List.fold (fun s num -> s * toInt num) 1
            factorsArePrime && cert.Modulus - 1 = apparentFactorisation

[<RequireQualifiedAccess>]
module Primality =

    let inline verifyComposite (i : ^int) (cert : CompositeCertificate< ^int>) : bool =
        match cert with
        | CompositeCertificate.Factor fact ->
            i % fact = LanguagePrimitives.GenericZero
            && fact < i && fact > LanguagePrimitives.GenericOne
        | CompositeCertificate.Fermat baseOfPower ->
            i > LanguagePrimitives.GenericOne &&
            Arithmetic.powerMod i baseOfPower (i - LanguagePrimitives.GenericOne) <> LanguagePrimitives.GenericOne
        | CompositeCertificate.SquareRoot sqrt ->
            Arithmetic.timesMod i sqrt sqrt = LanguagePrimitives.GenericOne
            && sqrt > LanguagePrimitives.GenericOne && sqrt < i - LanguagePrimitives.GenericOne
            && i % (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne) = LanguagePrimitives.GenericOne

    let verify (i : int) (cert : Certificate<int>) : bool =
        match cert with
        | Certificate.Zero -> i = LanguagePrimitives.GenericZero
        | Certificate.One -> i = LanguagePrimitives.GenericOne
        | Certificate.Composite cert -> verifyComposite i cert
        | Certificate.Prime cert -> PrimeCertificate.verify cert
