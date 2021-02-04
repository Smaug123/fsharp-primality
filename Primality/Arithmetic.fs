namespace Primality

[<RequireQualifiedAccess>]
module Arithmetic =

    /// Compute floor(sqrt(i)).
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

    /// Add two integers modulo `modulus`, being sure not to overflow the maximum size of the desired integer type.
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

    /// Multiply two integers modulo `modulus`, being sure not to overflow the maximum size of the desired integer type.
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

    /// Raise an integer to the power of another modulo `modulus`, being sure not to overflow the maximum size of the
    /// desired integer type.
    let inline powerMod (modulus : ^int) (a : ^int) (power : ^int) : ^int =
        if modulus = LanguagePrimitives.GenericZero then
            raise <| System.DivideByZeroException()
        if modulus < LanguagePrimitives.GenericZero then
            raise <| System.ArgumentOutOfRangeException "modulus"
        if power < LanguagePrimitives.GenericZero then
            raise <| System.ArgumentOutOfRangeException "power"
        if modulus = LanguagePrimitives.GenericOne then LanguagePrimitives.GenericZero else
        let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
        let rec go (power : 'int) (acc : 'int) (exponent : 'int) =
            if exponent = LanguagePrimitives.GenericZero then
                acc
            else
                let acc = if exponent % two = LanguagePrimitives.GenericOne then timesMod modulus acc power else acc
                go (timesMod modulus power power) acc (exponent >>> 1)

        go a LanguagePrimitives.GenericOne power

    /// A slow, inefficient integer factorisation algorithm.
    let factor (i : int) : int list =
        if i = 1 then [] else
        if i <= 0 then raise <| System.ArgumentOutOfRangeException "i"
        let rec go (startPoint : int) (acc : int list) (i : int) : int list =
            if startPoint * startPoint > i then i :: acc else
            if i = 1 then acc else
            if i % startPoint = 0 then go startPoint (startPoint :: acc) (i / startPoint) else
            go (startPoint + 2) acc i

        let rec extractTwo (acc : int list) (i : int) : int * (int list) =
            if i % 2 = 0 then extractTwo (2 :: acc) (i / 2) else i, acc

        let i, factors = extractTwo [] i
        if i = 1 then factors else
        go 3 factors i

