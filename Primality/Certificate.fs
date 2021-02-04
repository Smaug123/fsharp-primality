namespace Primality

[<RequireQualifiedAccess>]
module Certificate =

    let inline find (i : int) : Certificate<int> =
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        if i = LanguagePrimitives.GenericZero then Certificate.Zero
        elif i = LanguagePrimitives.GenericOne then Certificate.One
        elif i = two then Certificate.Prime PrimeCertificate.Two
        elif i % two = LanguagePrimitives.GenericZero then Certificate.Composite (CompositeCertificate.Factor 2)
        else

        // Find a Fermat witness for compositeness
        let fermatWitness =
            [| 2 ; 3 ; 5 ; 7 ; 11 ; 13 ; 17 ; 23 ; 29 ; 31 ; 37 ; 41 |]
            |> Array.tryFind (fun baseNum ->
                baseNum < i && Arithmetic.powerMod i baseNum (i - 1) <> 1
            )

        match fermatWitness with
        | Some baseNum -> Certificate.Composite (CompositeCertificate.Fermat baseNum)
        | None ->

        // It's looking pretty prime! Try and find a Pratt certificate.
        match PrimeCertificate.make i with
        | Ok cert -> Certificate.Prime cert
        | Error e ->
            failwithf "Hmm: %i (%+A)" i e