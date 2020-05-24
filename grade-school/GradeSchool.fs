module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let grade (number: int) (school: School): string list =
    match Map.tryFind number school with
    | Some students -> students
    | None -> []

let add (student: string) (gradeNumber: int) (school: School): School =
    let students = grade gradeNumber school
    let sortedStudents = student :: students |> List.sort
    Map.add gradeNumber sortedStudents school

let roster (school: School): string list =
    school
    |> Map.toSeq
    |> Seq.collect snd
    |> Seq.toList
