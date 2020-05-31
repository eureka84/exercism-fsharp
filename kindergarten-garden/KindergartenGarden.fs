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

let studentsList =
    [ "Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry" ]

let flatten (A: 'a [,]) = A |> Seq.cast<'a>

let studentCups (diagram: string) student =
    let index = List.findIndex ((=) student) studentsList

    let gardenMatrix =
        array2D (diagram.Split "\n")

    let startColumn = index * 2
    let endColumn = index * 2 + 1
    
    gardenMatrix.[0..1, startColumn..endColumn] |> flatten
    
let plants (diagram: string) (student: string) =
    studentCups diagram student
    |> Seq.map Plant.from
    |> Seq.toList