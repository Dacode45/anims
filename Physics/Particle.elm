module Physics.Particle exposing (..)

import Physics.Vector as Vector exposing (..)


{- State is a tuple of (Time, Displacement, Velocity) -}


type alias State =
    ( Float, Vec, Vec )
