namespace Primality.Internals

[<RequireQualifiedAccess>]
module internal Result =
    let get<'a, 'err> (r : Result<'a, 'err>) : 'a =
        match r with
        | Ok r -> r
        | Error e -> failwithf "Result.get failed due to an error case: %+A" e
