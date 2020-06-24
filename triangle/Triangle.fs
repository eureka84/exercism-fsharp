module Triangle

type Triangle =
    | Equilateral of l: double
    | Isosceles of b: double * l: double
    | Scalene of a: double * b: double * c: double

module Triangle =
    let validate (triangle: double list): double list option =
        if triangle.Length <> 3 then
            None
        else
            match triangle with
            | [ a; b; c ] when (a + b > c && a + c > b && b + c > a) -> Some triangle
            | _ -> None

    let create (triangle: double list): Triangle option =
        validate triangle
        |> Option.map (fun [ a; b; c ] ->
            match a, b, c with
            | _ when (a = b && b = c) -> Equilateral a
            | _ when a = b -> Isosceles(c, a)
            | _ when b = c -> Isosceles(a, b)
            | _ when a = c -> Isosceles(b, a)
            | _ -> Scalene(a, b, c))
    
    let isIsosceles (triangle: Triangle) =
        match triangle with
        | Scalene _ -> false
        | _ -> true
    
    let isEquilateral (triangle: Triangle) =
        match triangle with
        | Equilateral _ -> true
        | _ -> false
    
    let isScalene (triangle: Triangle) =
        match triangle with
        | Scalene _ -> true
        | _ -> false
    
let private test triangle predicate =
    Triangle.create triangle
    |> Option.map predicate
    |> Option.defaultValue false
    
let equilateral triangle =
    test triangle Triangle.isEquilateral

let isosceles triangle =
    test triangle Triangle.isIsosceles

let scalene triangle =
    test triangle Triangle.isScalene