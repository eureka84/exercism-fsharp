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

let zip4 (l1: string list) (l2: string list) (l3: string list) (l4: string list) =
    let zip (ll1: string list) (ll2: string list list): string list list =
        List.zip ll1 ll2 |> List.map (fun p -> fst p::snd p)
    List.zip l3 l4
    |> List.map (fun p -> fst p::snd p::[])
    |> zip l2
    |> zip l1
    
let getDigitsToConvert (line: string list): string list list =
    let chunked: string list list =
        line |> List.map (Seq.chunkBySize 3 >> Seq.toList >> List.map (fun x-> new string(x)))
    
    zip4 chunked.[0] chunked.[1] chunked.[2] chunked.[3]

let convertSingleLine (line: string list): string =
    getDigitsToConvert line
    |> Seq.map toDigit
    |> Seq.reduce (+)

let concat opt1 opt2 =
    match opt1, opt2 with
    | Some a, Some b -> Some(sprintf "%s,%s" a b)
    | _ -> None

let isValid (input: string list): string list option =
    let (|Has4Rows|_|) input = toOption (List.length input = 4)
    let lengthDivisibleBy3 str = String.length str % 3 = 0
    let (|ColumnsNumberIsDivisibleBy3|_|) (input: string list) =
        toOption (List.forall lengthDivisibleBy3 input)

    match input with
    | Has4Rows & ColumnsNumberIsDivisibleBy3 -> Some input
    | _ -> None
    
let convert (input: string list) =
    List.chunkBySize 4 input
    |> List.map (isValid >> Option.map convertSingleLine)
    |> List.reduce concat
