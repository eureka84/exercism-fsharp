module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let studentsForGrade (grade:int) (school:School): string list option =
    Map.tryFind grade school
    
let add (student: string) (grade: int) (school: School): School =
    match studentsForGrade grade school with
    | Some students ->
        let sortedStudents = student :: students |> List.sort
        Map.add grade sortedStudents school
    | None ->
        Map.add grade (student :: []) school

let roster (school: School): string list =
    school
    |> Map.toSeq
    |> Seq.collect snd
    |> Seq.toList    

let grade (number: int) (school: School): string list =
    match studentsForGrade number school with
    | Some students -> List.sort students
    | None -> []
