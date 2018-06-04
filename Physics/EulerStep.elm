module Physics.EulerStep exposing (..)

import Physics.Definitions exposing (..)
import Physics.Vector exposing (..)
import Physics.Particle exposing (..)


{-| eulerStep uses the euler method to incremetn state
   It takes in a deltat time and a particle and returns the next particle

   eulerStep gravity 1 (0, Vec 0 0 0, Vec 0 0 0)
-}
eulerStep : AccelerationFunction -> Float -> State -> State
eulerStep a dt ( t, r, v ) =
    let
        t_ =
            t + dt

        -- It's good to accumulate velocity first otherwise energy is injected into the system.
        v_ =
            v ^+^ a ( t, r, v ) ^* dt

        r_ =
            r ^+^ v_ ^* dt
    in
        ( t_, r_, v_ )
