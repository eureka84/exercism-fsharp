module Leap

let private isDivisibleBy divisor = fun x -> x % divisor = 0
let leapYear (year: int): bool =
       isDivisibleBy 400 year
    || ((isDivisibleBy 4 year ) && not (isDivisibleBy 100 year))
