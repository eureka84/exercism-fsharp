module KindergartenGarden

type Plant =
    | Radishes = 'R'
    | Clover = 'C'
    | Grass = 'G'
    | Violets = 'V'

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
    
let plants (diagram: string) (student: string): Plant list =
    studentCups diagram student
    |> Seq.map LanguagePrimitives.EnumOfValue
    |> Seq.toList