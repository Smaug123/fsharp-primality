namespace Primality

/// A proof of compositeness of an integer.
type CompositeCertificate<'int> =
    /// The integer fails Fermat's primality test to base `baseOfPower`.
    | Fermat of baseOfPower : 'int
    /// The integer n has a nontrivial square root of 1 (i.e. one which is neither 1 or -1) mod n.
    | SquareRoot of sqrt : 'int
    /// The integer has a nontrivial factor.
    | Factor of factor : 'int

/// A proof of primality of an integer.
[<RequireQualifiedAccess>]
type PrimeCertificate<'int> =
    /// The integer is 2.
    | Two
    /// The integer satisfies the given Pratt certificate.
    | Pratt of PrattCertificate<'int>

/// A certificate of the Pratt test, proving that the input was prime.
and PrattCertificate<'int> =
    {
        Modulus : 'int
        Base : 'int
        Factors : PrimeCertificate<'int> list
    }

/// A proof of the primality, compositeness, or otherwise of an integer.
type Certificate<'int> =
    /// The integer is prime.
    | Prime of PrimeCertificate<'int>
    /// The integer is composite.
    | Composite of CompositeCertificate<'int>
    /// The integer is 0, which is neither prime nor composite.
    | Zero
    /// The integer is 1, which is neither prime nor composite.
    | One