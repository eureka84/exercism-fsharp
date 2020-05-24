module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty   

let add (student: string) (grade: int) (school: School): School =
    if Map.containsKey grade school then
        let students = Map.find grade school
        Map.add grade (student::students) school
    else
        Map.add grade (student::[]) school
        

let roster (school: School): string list =
    let folder acc _ v = List.sort v |> (@) acc  
    Map.fold folder [] school

let grade (number: int) (school: School): string list =
    if Map.containsKey number school then 
        Map.find number school |> List.sort
    else
        []