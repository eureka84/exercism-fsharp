module RobotSimulator

open System
open System.Reflection

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

let create direction position =
    { direction = direction
      position = position }

let changeDirection robot direction =
    { robot with direction = direction }

let private rotateRight robot =
    match robot.direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North
    |> changeDirection robot

let private rotateLeft robot =
    match robot.direction with
    | North -> West
    | East -> North
    | South -> East
    | West -> South
    |> changeDirection robot

let changePosition robot position =
    { robot with position = position }

let private moveForward robot =
    let { direction = d; position = x, y } = robot
    match d with
    | North -> x, y + 1 
    | East -> x + 1, y 
    | South -> x, y - 1 
    | West -> x - 1, y
    |> changePosition robot

type Command =
    | RotateRight
    | RotateLeft
    | MoveForward

module Command =
    let parse = function
        | 'R' -> Some RotateRight
        | 'L' -> Some RotateLeft
        | 'A' -> Some MoveForward
        | _ -> None 

let moveRobot (robot: Robot) (command: Command) =
    match command with
    | RotateRight -> rotateRight robot
    | RotateLeft -> rotateLeft robot
    | MoveForward -> moveForward robot

let sequence (x: 'a option seq): 'a seq option =
    let folder (acc: 'a seq option) (el: 'a option): 'a seq option =
        match acc, el with
        | Some sequence, Some a ->
            Some (seq {
                    yield! sequence
                    yield a
                 })
        | Some _, None
        | None, Some _
        | None, None -> None
    Seq.fold folder (Some Seq.empty) x

let safeMove (instructions: string) (robot: Robot): Robot option =
    instructions
    |> Seq.map Command.parse
    |> sequence
    |> Option.map (Seq.fold moveRobot robot)

let move (instructions: string) (robot: Robot) =
    match safeMove instructions robot with
    | Some r -> r
    | None -> raise (Exception "Failed to parse")
