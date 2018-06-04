module Physics.Definitions exposing (..)

import Physics.Particle exposing (State)
import Physics.Vector exposing (..)


{- AccelerationFunction describes a method
   to determine the acceleration of a particle

-}


type alias AccelerationFunction =
    State -> Vec
