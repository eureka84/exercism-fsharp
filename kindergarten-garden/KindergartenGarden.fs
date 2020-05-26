module KindergartenGarden

type Plant =
    | Radishes
    | Clover
    | Grass
    | Violets
    static member from(letter) =
        match letter with
        | 'R' -> Radishes
        | 'C' -> Clover
        | 'G' -> Grass
        | 'V' -> Violets

let students =
    seq {
        "Alice"
        "Bob"
        "Charlie"
        "David"
        "Eve"
        "Fred"
        "Ginny"
        "Harriet"
        "Ileana"
        "Joseph"
        "Kincaid"
        "Larry" }



let splitIn2Lines (s: string) =
    let lines: string [] = s.Split "\n"
    lines.[0], lines.[1]
let chunkBy2 (s: string) = Seq.chunkBySize 2 s

let concatPairOfArray (pair: char [] * char []) =
    Array.concat
        [ (fst pair)
          (snd pair) ]
let toPlants (a: char []) = Array.map Plant.from a
       

let plants (diagram: string) (student: string) =
    let firstLine, secondLine = splitIn2Lines diagram

    let studentsPlants =
        Seq.zip (chunkBy2 firstLine) (chunkBy2 secondLine)
        |> Seq.map concatPairOfArray
        |> Seq.map toPlants

    Seq.zip students studentsPlants
    |> Seq.find (fun x -> (fst x) = student)
    |> snd
    |> Array.toList
