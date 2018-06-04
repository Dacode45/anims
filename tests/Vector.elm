module Vector exposing (..)

import Physics.Vector exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, tuple3, int, float, list, string)
import Test exposing (..)


testBasics : Test
testBasics =
    describe "vector math"
        [ fuzz2 t3 t3 "adds two given vectors" <|
            \( ax, ay, az ) ( bx, by, bz ) ->
                let
                    a =
                        Vec ax ay az

                    b =
                        Vec bx by bz
                in
                    a ^+^ b |> Expect.equal (Vec (ax + bx) (ay + by) (az + bz))
        , fuzz2 t3 t3 "subtracts two given vectors" <|
            \( ax, ay, az ) ( bx, by, bz ) ->
                let
                    a =
                        Vec ax ay az

                    b =
                        Vec bx by bz
                in
                    a ^-^ b |> Expect.equal (Vec (ax - bx) (ay - by) (az - bz))
        , fuzz2 float t3 "left scalar multiplication" <|
            \a ( bx, by, bz ) ->
                let
                    b =
                        Vec bx by bz
                in
                    a *^ b |> Expect.equal (Vec (a * bx) (a * by) (a * bz))
        , fuzz2 t3 float "right scalar multiplication" <|
            \( ax, ay, az ) b ->
                let
                    a =
                        Vec ax ay az
                in
                    a ^* b |> Expect.equal (Vec (ax * b) (ay * b) (az * b))
        , fuzz2 t3 float "scalar division" <|
            \( ax, ay, az ) b ->
                let
                    a =
                        Vec ax ay az
                in
                    if b == 0 then
                        Expect.equal 0 0
                    else
                        a ^/ b |> Expect.equal (Vec (ax / b) (ay / b) (az / b))
        , fuzz2 t3 t3 "dot product given vectors" <|
            \( ax, ay, az ) ( bx, by, bz ) ->
                let
                    a =
                        Vec ax ay az

                    b =
                        Vec bx by bz
                in
                    a <.> b |> Expect.equal ((ax * bx) + (ay * by) + (az * bz))
        , fuzz2 t3 t3 "cross two given vectors" <|
            \( ax, ay, az ) ( bx, by, bz ) ->
                let
                    a =
                        Vec ax ay az

                    b =
                        Vec bx by bz
                in
                    a >< b |> Expect.equal (Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx))
        ]


t3 : Fuzzer ( Float, Float, Float )
t3 =
    tuple3 ( float, float, float )
