module OcrNumbers

let toDigit = function 
     | [ " _ "; "| |"; "|_|"; "   " ] -> "0" 
     | [ "   "; "  |"; "  |"; "   " ] -> "1"
     | [ " _ "; " _|"; "|_ "; "   " ] -> "2"
     | [ " _ "; " _|"; " _|"; "   " ] -> "3"
     | [ "   "; "|_|"; "  |"; "   " ] -> "4"
     | [ " _ "; "|_ "; " _|"; "   " ] -> "5"
     | [ " _ "; "|_ "; "|_|"; "   " ] -> "6"
     | [ " _ "; "  |"; "  |"; "   " ] -> "7"
     | [ " _ "; "|_|"; "|_|"; "   " ] -> "8"
     | [ " _ "; "|_|"; " _|"; "   " ] -> "9"
     | _ -> "?"

let toOption b =
    if b then Some() else None

let isValid (input: string list): string list option =
    let (|Has4Rows|_|) input = toOption (List.length input = 4)
    let lengthDivisibleBy3 str = String.length str % 3 = 0
    let (|ColumnsNumberIsDivisibleBy3|_|) (input: string list) =
        toOption (List.forall lengthDivisibleBy3 input)

    match input with
    | Has4Rows & ColumnsNumberIsDivisibleBy3 -> Some input
    | _ -> None

let toList (slice: char [,]) =
    let sliceToString slice =
        [ for i in slice -> i ]
        |> List.toArray
        |> fun x -> new string(x)
    [ sliceToString slice.[0, 0..]
      sliceToString slice.[1, 0..]
      sliceToString slice.[2, 0..]
      sliceToString slice.[3, 0..] ]

let getDigitsToConvert (line: string list) =
    let charMatrix: char [,] =
        List.map (fun (x: string) -> x.ToCharArray()) line |> array2D

    let rec slice (acc: char [,] list) (rem: char [,]) =
        match rem with
        | a when a.Length = 0 -> acc
        | _ -> slice (acc @ [ rem.[0..3, 0..2] ]) rem.[0..3, 3..]

    slice [] charMatrix |> List.map toList

let convertSingleLine (line: string list): string =
    getDigitsToConvert line
    |> Seq.map toDigit
    |> Seq.reduce (+)

let concat opt1 opt2 =
    match opt1, opt2 with
    | Some a, Some b -> Some(sprintf "%s,%s" a b)
    | _ -> None

let convert (input: string list) =
    List.chunkBySize 4 input
    |> List.map (isValid >> Option.map convertSingleLine)
    |> List.reduce concat
