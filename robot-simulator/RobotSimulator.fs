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

let moveRobot (robot: Robot) (command: char) =
    match command with
    | 'R' -> rotateRight robot
    | 'L' -> rotateLeft robot
    | 'A' -> moveForward robot

let move (instructions: string) (robot: Robot) =
    instructions |> Seq.fold moveRobot robot
