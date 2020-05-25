module Bob

let private toOption bool =
    if bool then Some() else None

let response (input: string): string =
    let (|Shouting|_|) (input: string) =
        let isUpper = input = input.ToUpper()

        let containsLetters =
            [ 'a' .. 'z' ]
            |> List.tryFind (fun c -> input.ToLower().Contains(c))
            |> Option.isSome
        (isUpper && containsLetters) |> toOption

    let (|AskingQuestions|_|) (input: string) =
        input.Trim().EndsWith("?") |> toOption

    let (|Silence|_|) (input: string) =
        input.Trim()
        |> String.length = 0
        |> toOption

    match input with
    | Silence -> "Fine. Be that way!"
    | Shouting & AskingQuestions -> "Calm down, I know what I'm doing!"
    | Shouting -> "Whoa, chill out!"
    | AskingQuestions -> "Sure."
    | _ -> "Whatever."
