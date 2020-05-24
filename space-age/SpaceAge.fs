module SpaceAge

open System.ComponentModel

type Planet =
    | Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    member this.timePassingRatioToEarth() =
        match this with
            | Mercury -> 0.2408467
            | Venus -> 0.61519726
            | Earth -> 1.0
            | Mars -> 1.8808158
            | Jupiter -> 11.862615
            | Saturn -> 29.4479498
            | Uranus -> 84.016846
            | Neptune -> 164.79132

let age (planet: Planet) (seconds: int64): float =
    let aDayInSeconds = 60 * 60 * 24
    let aYearInSeconds = float aDayInSeconds * float 365.25
    let aYearOnThePlanet = aYearInSeconds * planet.timePassingRatioToEarth() 
    float seconds / aYearOnThePlanet