module Clock

type Clock =
    { Hours: int
      Minutes: int }

let create (hours: int) (minutes: int): Clock =
    let m, dh = 
        let m1 = minutes % 60
        let dh1 = minutes / 60
        if m1 < 0 then
            60 + m1, dh1 - 1
        else
            m1, dh1
    let h =
        let h1 = (hours + dh) % 24
        if h1 < 0 then
            24 + h1
        else
            h1
    { Hours = h
      Minutes = m }

let add minutes (clock: Clock) =
    create clock.Hours (clock.Minutes + minutes)

let subtract minutes (clock: Clock) =
    create clock.Hours (clock.Minutes - minutes)

let display (clock: Clock) =
    sprintf "%02i:%02i" clock.Hours clock.Minutes
