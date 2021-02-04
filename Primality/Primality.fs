namespace Primality

[<RequireQualifiedAccess>]
module Primality =

    let inline isPrime (i : int) : Certificate<int> =
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        if i = LanguagePrimitives.GenericZero then Certificate.Zero
        elif i = LanguagePrimitives.GenericOne then Certificate.One
        elif i = two then Certificate.Prime PrimeCertificate.Two
        elif i % two = LanguagePrimitives.GenericZero then Certificate.Composite (CompositeCertificate.Factor 2)
        else
            // Attempt to prove composite

            failwith ""