module ReviewConfig exposing (config)

import OnlyImportOutside
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ OnlyImportOutside.rule
        { constrainedModulePrefixes = [ "Data" ]
        , allowedModulePrefixes = [ "Extra" ] -- for illustration only; you can leave it `[]`
        }
    ]
