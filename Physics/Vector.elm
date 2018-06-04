module Physics.Vector exposing (..)

{-| Simple Vector class. In xyz format
-}


type alias Vec =
    { x : Float
    , y : Float
    , z : Float
    }


{-| Adds two vectors

    a ^+^ b
-}
(^+^) : Vec -> Vec -> Vec
(^+^) a b =
    Vec (a.x + b.x) (a.y + b.y) (a.z + b.z)


{-| Subtracts two vectors

    a ^-^ b
-}
(^-^) : Vec -> Vec -> Vec
(^-^) a b =
    Vec (a.x - b.x) (a.y - b.y) (a.z - b.z)


{-| Scalar multiplication where left operand is the scalar

    a *^ b
-}
(*^) : Float -> Vec -> Vec
(*^) t a =
    Vec (a.x * t) (a.y * t) (a.z * t)


{-| Scalar multiplication where right operand is the scalar

    a ^* b
-}
(^*) : Vec -> Float -> Vec
(^*) a t =
    t *^ a


{-| Scalar division

    a ^+^ b
-}
(^/) : Vec -> Float -> Vec
(^/) a t =
    Vec (a.x / t) (a.y / t) (a.z / t)


{-| Dot product

    a <.> b
-}
(<.>) : Vec -> Vec -> Float
(<.>) a b =
    a.x * b.x + a.y * b.y + a.z * b.z


{-| Cross Product

    a >< b
-}
(><) : Vec -> Vec -> Vec
(><) a b =
    let
        x =
            a.y * b.z - a.z * b.y

        y =
            a.z * b.x - a.x * b.z

        z =
            a.x * b.y - a.y * b.x
    in
        Vec x y z


{-| Magnitude of a vector

    magnitude a = sqrt (x*x + y*y + z*z)
-}
magnitude : Vec -> Float
magnitude { x, y, z } =
    sqrt (x * x + y * y + z * z)


{-| Squared magnitude of a vector

    sqMagnitude a = x*x + y*y + z*z
-}
sqMagnitude : Vec -> Float
sqMagnitude { x, y, z } =
    x * x + y * y + z * z


{-| Vector with all zero components
-}
zeroV : Vec
zeroV =
    Vec 0 0 0


{-| Unit vector on x axis
-}
unitX : Vec
unitX =
    Vec 1 0 0


{-| Unit vector on y axis
-}
unitY : Vec
unitY =
    Vec 0 1 0


{-| Unit vector on z axis
-}
unitZ : Vec
unitZ =
    Vec 0 0 1


{-| Negates a vector
    negateV = -1 *^ a
-}
negateV : Vec -> Vec
negateV a =
    -1 *^ a


{-| Adds a list of vectors
-}
sumV : List Vec -> Vec
sumV vecs =
    List.foldr (^+^) zeroV vecs


{-| Converts a tuple of Floats to a Vector
-}
tupleV : ( Float, Float, Float ) -> Vec
tupleV ( x, y, z ) =
    Vec x y z


{-| Converts a vector into a tuple of floats
-}
vecT : Vec -> ( Float, Float, Float )
vecT { x, y, z } =
    ( x, y, z )
