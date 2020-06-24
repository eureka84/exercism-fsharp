module Triangle

type TriangleKind =
    | Equilateral
    | Isosceles
    | Scalene

module TriangleKind =
    let private validate (triangle: double list): (double * double * double) option =
        match List.sort triangle with
        | [ a; b; c ] when a + b > c -> Some(a, b, c)
        | _ -> None

    let ofGiven (triangle: double list): TriangleKind option =
        validate triangle
        |> Option.map (fun (a, b, c) ->
            match a, b, c with
            | _ when a = b && b = c -> Equilateral
            | _ when a = b || b = c || a = c -> Isosceles
            | _ -> Scalene)

    let isIsosceles (triangle: TriangleKind) =
        match triangle with
        | Equilateral
        | Isosceles -> true
        | Scalene -> false

    let isEquilateral (triangle: TriangleKind) =
        match triangle with
        | Equilateral _ -> true
        | _ -> false

    let isScalene (triangle: TriangleKind) =
        match triangle with
        | Scalene _ -> true
        | _ -> false

let private test predicate triangle =
    TriangleKind.ofGiven triangle
    |> Option.map predicate
    |> Option.defaultValue false

let equilateral triangle =
    triangle |> test TriangleKind.isEquilateral

let isosceles triangle =
    triangle |> test TriangleKind.isIsosceles

let scalene triangle =
    triangle |> test TriangleKind.isScalene
