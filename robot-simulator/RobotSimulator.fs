module RobotSimulator

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

let private rotateRight robot =
    match robot.direction with
    | North -> { robot with direction = East }
    | East -> { robot with direction = South }
    | South -> { robot with direction = West }
    | West -> { robot with direction = North }
    
let private rotateLeft robot =
    match robot.direction with
    | North -> { robot with direction = West }
    | East -> { robot with direction = North }
    | South -> { robot with direction = East }
    | West -> { robot with direction = South }

let private moveForward robot =
    match robot with
    | { direction = North; position = x, y } ->
        { robot with position = x,  y + 1 }
    | { direction = East; position = x, y } ->
        { robot with position = x  + 1, y }
    | { direction = South; position = x, y } ->
        { robot with position = x, y - 1 }
    | { direction = West; position = x, y } ->
        { robot with position = x - 1, y }

let moveRobot (robot: Robot) (command: char) =
    match command with
    | 'R' -> rotateRight robot
    | 'L' -> rotateLeft robot
    | 'A' -> moveForward robot
let move (instructions: string) (robot: Robot) =
    instructions
    |> Seq.fold moveRobot robot 
    
