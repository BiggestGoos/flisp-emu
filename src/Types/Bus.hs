module Types.Bus where

import Types.Byte

type Bus = Maybe Byte

add :: Bus -> Bus -> Bus
add Nothing Nothing = Nothing
add value Nothing = value
add Nothing value = value
add _ _ = error "More than one value in the bus can't be Nothing!"
