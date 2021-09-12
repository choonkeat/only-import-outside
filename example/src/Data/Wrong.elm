module Data.Wrong exposing (Wrong)

{-| All 3 imports are invalid according to

    OnlyImportOutside.rule
        { constrainedModulePrefixes = [ "Data" ]
        , allowedModulePrefixes = [ "Extra" ]
        }

-}

import Data.Animal
import Data.Thing
import Page


type Wrong
    = Wrong
