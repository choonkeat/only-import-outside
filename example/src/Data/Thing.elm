module Data.Thing exposing (Thing)

{-| Imports are acceptable

`import Data.Thing.Gadget` is ok since it's under this module namespace `Data.Thing`
`import Extra.Time` is ok since `Extra` is allowed through `allowedModulePrefixes = [ "Extra" ]`

-}

import Data.Thing.Gadget
import Extra.Time


type Thing
    = Thing
