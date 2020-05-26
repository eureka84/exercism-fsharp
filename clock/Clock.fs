module Clock

type Clock =
    { Hours: int
      Minutes: int }

let create (hours: int) (minutes: int): Clock =
    let rec loop h m =
        match h, m with
        | h, m when m >= 60 -> loop (h + 1) (m - 60) 
        | h, m when m < 0 -> loop (h - 1)  (m + 60)
        | h, m when h >= 24 -> loop (h - 24) m
        | h, m when h < 0 -> loop (h + 24) m
        | _ -> { Hours = h ; Minutes = m }
    loop hours minutes

let add minutes (clock: Clock) =
    create clock.Hours (clock.Minutes + minutes)

let subtract minutes (clock: Clock) =
    create clock.Hours (clock.Minutes - minutes)

let display (clock: Clock) =
    sprintf "%02i:%02i" clock.Hours clock.Minutes
