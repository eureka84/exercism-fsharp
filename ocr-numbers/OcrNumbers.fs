module OcrNumbers

let toDigit =
    function
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

let digitWidth = 3
let digitHeight = 4

let zip4 (l1: string list) (l2: string list) (l3: string list) (l4: string list) =
    let zip ll1 ll2 =
        List.zip ll1 ll2 |> List.map (fun p -> fst p :: snd p)
    List.zip l3 l4
    |> List.map (fun p -> fst p :: snd p :: [])
    |> zip l2
    |> zip l1

let getDigitsToConvert (number: string list): string list list =
    let chunked: string list list =
        number
        |> List.map
            (Seq.chunkBySize digitWidth
             >> Seq.map (fun x -> new string(x))
             >> Seq.toList)

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
    let validRows input = List.length input = digitHeight
    let validCols (input: string list) =
        List.forall (fun x -> String.length x % digitWidth = 0) input

    if validRows input && validCols input
    then Some input
    else None

let convert (input: string list) =
    List.chunkBySize 4 input
    |> List.map (isValid >> Option.map convertSingleLine)
    |> List.reduce concat
