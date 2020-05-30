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

let convertValidCandidate input: string option =
    List.tryFindIndex ((=) input) numbers
    |> Option.map string
    |> Option.orElse (Some "?")

let toOption b =
    if b then Some() else None

let isValid (input: string list): string list option =
    let (|Has4Rows|_|) input = toOption (List.length input = 4)

    let lengthDivisibleBy3 str = String.length str % 3 = 0

    let (|ItsColumnsAreAMultipleOf3|_|) (input: string list) =
        toOption (List.forall lengthDivisibleBy3 input)

    match input with
    | Has4Rows & ItsColumnsAreAMultipleOf3 -> Some input
    | _ -> None

let chunkBySize n str: string seq =
    Seq.chunkBySize n str |> Seq.map (fun arr -> new string(arr))

let zipToList (a: string seq) (b: string seq): string list seq =
    Seq.zip a b
    |> Seq.map (fun x ->
        [ fst x
          snd x ])

let zipToList2 (b: string seq) (a: string list seq): string list seq =
    Seq.zip a b |> Seq.map (fun x -> fst x @ [ snd x ])

let splitNumbers input: string list seq =
    let chunked = input |> List.map (chunkBySize 3)
    (zipToList chunked.[0] chunked.[1])
    |> zipToList2 chunked.[2]
    |> zipToList2 chunked.[3]

let sequence (x: 'a option seq): 'a seq option =
    let folder (acc: 'a seq option) (el: 'a option): 'a seq option =
        match acc, el with
        | Some sequence, Some a ->
            Some
                (seq {
                    yield a
                    yield! sequence
                 })
        | Some _, None
        | None, Some _
        | None, None -> None
    Seq.fold folder (Some Seq.empty) x

let convertSingleLine (line: string list): string option =
    splitNumbers line
    |> Seq.map convertValidCandidate
    |> sequence
    |> Option.map (fun y -> Seq.rev y |> Seq.reduce (+))

let splitLines (input: string list): string list list =
    let rec loop (acc: string list List) (rem: string list) =
        match rem with
        | l when l.Length > 4 -> loop (acc @ [ List.take 4 l ]) (List.skip 4 l)
        | l -> acc @ [ l ]
    loop [] input

let concat opt1 opt2 =
    match opt1, opt2 with
    | Some a, Some b -> Some (sprintf "%s,%s" a b)
    | _ -> None

let convert (input: string list) =
    splitLines input
    |> List.map (isValid >> Option.map convertSingleLine)
    |> sequence
    |> Option.bind (Seq.rev >> Seq.reduce concat)
