module Physics.Common.Accelrations exposing (..)

import Physics.Definitions exposing (..)
import Physics.Vector exposing (..)
import Physics.Constants exposing (..)


{-| Equation of a satelite in motion
-}
satelite : AccelerationFunction
satelite ( t, r, v ) =
    let
        u =
            negateV r ^/ magnitude r
    in
        (g / magnitude r ^ 2) *^ u


dampedDrivenOsc :
    Float
    -- damping constant
    -> Float
       -- drive amplitude
    -> Float
       -- drive frequency
    -> AccelerationFunction
dampedDrivenOsc beta driveAmp omega ( t, r, v ) =
    let
        forceDamp =
            (-beta) *^ v

        forceDrive =
            (driveAmp * cos (omega * t)) *^ unitX

        forceSpring =
            (-k) *^ r

        mass =
            1

        k =
            1

        -- spring constant
    in
        (forceDamp ^+^ forceDrive ^+^ forceSpring) ^/ mass
