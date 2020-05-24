module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
    match Map.tryFind grade school with
    | Some students ->
        Map.add grade (student :: students) school
    | None ->
        Map.add grade (student :: []) school

let roster (school: School): string list =
    let folder acc _ v = List.sort v |> (@) acc
    Map.fold folder [] school

let grade (number: int) (school: School): string list =
    match Map.tryFind number school with
    | Some students -> List.sort students
    | None -> []
