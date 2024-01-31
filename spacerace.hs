module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving Eq

secondsInYear = 31557600.0

ageOn :: Planet -> Float -> Float
ageOn planet
  | planet == Mercury = (\x -> x / (0.2408467 * secondsInYear))
  | planet == Venus = (\x -> x / (0.61519726 * secondsInYear))
  | planet == Earth = (\x -> x / (1 * secondsInYear))
  | planet == Mars = (\x -> x / (1.8808158 * secondsInYear))
  | planet == Jupiter = (\x -> x / (11.862615 * secondsInYear))
  | planet == Saturn = (\x -> x / (29.447498 * secondsInYear))
  | planet == Uranus = (\x -> x / (84.016846 * secondsInYear))
  | planet == Neptune = (\x -> x / (164.79132 * secondsInYear))