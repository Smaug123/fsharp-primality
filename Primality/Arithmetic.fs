namespace Primality

[<RequireQualifiedAccess>]
module Arithmetic =

    let inline sqrt (i : ^a) =
        if i <= LanguagePrimitives.GenericOne then i else
        let rec go start =
            let next = start + LanguagePrimitives.GenericOne
            let sqr = next * next
            if sqr < LanguagePrimitives.GenericZero then
                // Overflow attempted, so the sqrt is between start and next
                start
            elif i < sqr then
                start
            elif i = sqr then next
            else go next
        go LanguagePrimitives.GenericOne

    /// Find Hcf, A, B s.t. A * a + B * b = Hcf, and Hcf is the highest common factor of a and b.
    let inline euclideanAlgorithm (a : ^a) (b : ^a) : {| Hcf : ^a ; A : ^a ; B : ^a |} =
        let rec go rMin1 r sMin1 s tMin1 t =
            if r = LanguagePrimitives.GenericZero then {| Hcf = rMin1 ; A = sMin1 ; B = tMin1 |} else
            let newQ = rMin1 / r
            go r (rMin1 - newQ * r) s (sMin1 - newQ * s) t (tMin1 - newQ * t)

        let maxA = max a b
        let minB = min a b
        let result = go maxA minB LanguagePrimitives.GenericOne LanguagePrimitives.GenericZero LanguagePrimitives.GenericZero LanguagePrimitives.GenericOne
        if a = maxA then result else {| Hcf = result.Hcf ; A = result.B ; B = result.A |}

    let inline addMod (modulus : ^a) (x1 : ^a) (x2 : ^a) : ^a =
        let res = x1 + x2
        if res >= LanguagePrimitives.GenericZero then
            res % modulus
        else
            let x1, x2 = max x1 x2, min x1 x2
            let x1 = x1 % modulus
            let x2 = x2 % modulus
            let res = x1 + x2
            if res >= LanguagePrimitives.GenericZero then
                res % modulus
            else
                ((x1 - modulus) + x2) % modulus

    let inline timesMod (modulus : ^a) (x1 : ^a) (x2 : ^a) : ^a =
        let x1 = if x1 < LanguagePrimitives.GenericZero then (x1 % modulus) + modulus else x1 % modulus
        let x2 = if x2 < LanguagePrimitives.GenericZero then (x2 % modulus) + modulus else x2 % modulus
        if x1 = LanguagePrimitives.GenericOne then x2 else
        if x2 = LanguagePrimitives.GenericOne then x1 else
        if x1 = LanguagePrimitives.GenericZero then x1 else
        if x2 = LanguagePrimitives.GenericZero then x2 else

        let mutable acc = LanguagePrimitives.GenericZero
        let mutable max = max x1 x2
        let mutable min = min x1 x2
        let two : ^a = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        while min > LanguagePrimitives.GenericZero do
            let rem = min % two
            min <- min >>> 1
            if rem <> LanguagePrimitives.GenericZero then
                acc <- addMod modulus max acc
            max <- addMod modulus max max
            if min > max then
                let temp = max
                max <- min
                min <- temp

        acc

    let powerMod (a : 'int) (b : 'int) (modulus : 'int) : 'int =
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        let rec go (power : 'int) (acc : 'int) (exponent : 'int) =
            if exponent = LanguagePrimitives.GenericZero then
                acc
            else
                let acc = if exponent % two = LanguagePrimitives.GenericOne then (acc * power) % modulus else acc
                go ((power * power) % modulus) acc (exponent >>> 1)

        go a LanguagePrimitives.GenericOne b


