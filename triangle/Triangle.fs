module Triangle

type TrinagleKind = Equilateral | Isosceles | Scalene 

module TrinagleKind =
    let validate (triangle: double list): (double*double*double) option =
        if triangle.Length <> 3 then
            None
        else
            match List.sort triangle with
            | [ a; b; c ] when a + b > c -> Some (a, b, c)
            | _ -> None

    let ofGiven (triangle: double list): TrinagleKind option =
        validate triangle
        |> Option.map (fun (a , b, c)  ->
            match a, b, c with
            | _ when a = b && b = c -> Equilateral
            | _ when a = b || b = c || a = c -> Isosceles
            | _ -> Scalene)
    
    let isIsosceles (triangle: TrinagleKind) =
        match triangle with
        | Equilateral | Isosceles -> true
        | Scalene -> false
    
    let isEquilateral (triangle: TrinagleKind) =
        match triangle with
        | Equilateral _ -> true
        | _ -> false
    
    let isScalene (triangle: TrinagleKind) =
        match triangle with
        | Scalene _ -> true
        | _ -> false
    
let private test predicate triangle  =
    TrinagleKind.ofGiven triangle
    |> Option.map predicate
    |> Option.defaultValue false
    
let equilateral triangle =
    triangle |> test TrinagleKind.isEquilateral

let isosceles triangle =
    triangle |> test TrinagleKind.isIsosceles

let scalene triangle =
    triangle |> test TrinagleKind.isScalene