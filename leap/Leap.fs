module Leap

let leapYear (year: int): bool =
    let isDivisibleBy divisor = year % divisor = 0
    isDivisibleBy 400  || (isDivisibleBy 4) && not (isDivisibleBy 100)
