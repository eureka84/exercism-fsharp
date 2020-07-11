module ErrorHandling

open System

let handleErrorByThrowingException() = failwith "Exception"

let handleErrorByReturningOption (input: string) =
    try
        Some (int input)
    with
        _ -> None

let handleErrorByReturningResult (input: string) =
    try
        Ok (int input)
    with
        _ -> Error "Could not convert input to integer"

let bind (switchFunction: 'a -> Result<'a, 'e>) (twoTrackInput: Result<'a, 'e>): Result<'a, 'e> =
    match twoTrackInput with
    | Ok x -> switchFunction x
    | Error _ -> twoTrackInput 

let cleanupDisposablesWhenThrowingException (resource: IDisposable) =
    try
        failwith "Exception"
    finally
        resource.Dispose()