module OcrNumbers


let numbers = [
    [ " _ ";
      "| |";
      "|_|";
      "   " ];
     [ "   ";
      "  |";
      "  |";
      "   " ];
    [ " _ ";
      " _|";
      "|_ ";
      "   " ]
    [ " _ ";
      " _|";
      " _|";
      "   " ]
    [ "   ";
      "|_|";
      "  |";
      "   " ]
    [ " _ ";
      "|_ ";
      " _|";
      "   " ]
    [ " _ ";
      "|_ ";
      "|_|";
      "   " ];
    [ " _ ";
      "  |";
      "  |";
      "   " ]
    [ " _ ";
      "|_|";
      "|_|";
      "   " ]
    [ " _ ";
      "|_|";
      " _|";
      "   " ]
]

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
    
let toList (arr: char [,]) =
    let sliceToString slice =
         [ for i in slice -> i] |> List.toArray |> fun x -> new string(x)
    [
        sliceToString arr.[0, 0..]
        sliceToString arr.[1, 0..] 
        sliceToString arr.[2, 0..] 
        sliceToString arr.[3, 0..] 
    ]
let splitNumbers line =
    let charMatrix =
        line
        |> List.map (fun (x: string) -> x.ToCharArray()) 
        |> array2D
    
    let rec loop (acc: char [,] list) (rem: char [, ]) =
        match rem with
        | a when a.Length = 0 -> acc
        | _ -> loop (acc @ [rem.[0..3, 0..2]]) rem.[0..3, 3..]
        
    loop [] charMatrix
    |> List.map toList
   
let toNumber (input: string list): string =
    List.tryFindIndex ((=) input) numbers
    |> Option.map string
    |> Option.defaultValue "?"
    
let convertSingleLine (line: string list): string =
    splitNumbers line
    |> Seq.map toNumber
    |> Seq.reduce (+)

let concat opt1 opt2 =
    match opt1, opt2 with
    | Some a, Some b -> Some (sprintf "%s,%s" a b)
    | _ -> None

let convert (input: string list) =
    List.chunkBySize 4 input
    |> List.map (isValid >> Option.map convertSingleLine)
    |> List.reduce concat 
