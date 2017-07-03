module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (secondsOfYear * scale planet) where
    secondsOfYear = 31557600
    scale Mercury = 0.2408467
    scale Venus = 0.61519726
    scale Earth = 1
    scale Mars = 1.8808158
    scale Jupiter = 11.862615
    scale Saturn = 29.447498
    scale Uranus = 84.016846
    scale Neptune = 164.79132
