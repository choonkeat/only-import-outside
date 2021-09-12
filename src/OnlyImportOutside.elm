module OnlyImportOutside exposing (rule)

{-| Enforces that certain module prefixes, e.g. `Data`, can only import modules from outside your codebase

@docs rule

-}

import Dict
import Elm.Syntax.Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Error, Rule)
import Set


type alias Params =
    { constrainedModulePrefixes : List String
    , allowedModulePrefixes : List String
    }


type alias ImportedModuleName =
    String


type alias ModuleContext =
    { moduleKey : Review.Rule.ModuleKey
    , moduleName : String
    , importRanges : List ( ImportedModuleName, Range )
    }


type alias ProjectContext =
    { moduleContextList : List ModuleContext
    }


{-| `constrainedModulePrefixes` refers to the modules that you want to enforce, e.g. `[ "Data" ]`

`allowedModulePrefixes` should be `[]` unless there are special module prefixes within your
own repository that you allow `constrainedModulePrefixes` modules to import from.

    OnlyImportOutside.rule
        { constrainedModulePrefixes = [ "Data" ]
        , allowedModulePrefixes = []
        }

-}
rule : Params -> Rule
rule params =
    Review.Rule.newProjectRuleSchema "OnlyImportFromOutside"
        { moduleContextList = [] }
        |> Review.Rule.withModuleVisitor moduleVisitor
        |> Review.Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Review.Rule.withFinalProjectEvaluation (finalEvaluationForProject params)
        |> Review.Rule.fromProjectRuleSchema


fromProjectToModule : Review.Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule moduleKey node pctx =
    let
        moduleName =
            Elm.Syntax.Node.value node
                |> String.join "."
    in
    { moduleKey = moduleKey
    , moduleName = moduleName
    , importRanges = []
    }


fromModuleToProject : Review.Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey node mctx =
    { moduleContextList = [ mctx ]
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts pctx1 pctx2 =
    { moduleContextList = pctx1.moduleContextList ++ pctx2.moduleContextList
    }


finalEvaluationForProject : Params -> ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject params pctx =
    let
        isPrefixedByAnyOf prefixes longstr =
            List.filter (\prefix -> String.startsWith prefix longstr) prefixes
                |> (not << List.isEmpty)

        seenModules =
            pctx.moduleContextList
                |> List.foldl (\{ moduleName } set -> Set.insert moduleName set) Set.empty
    in
    pctx.moduleContextList
        |> List.filter (\{ moduleName } -> isPrefixedByAnyOf params.constrainedModulePrefixes moduleName)
        |> List.map
            (\{ moduleKey, moduleName, importRanges } ->
                List.filterMap
                    (\( importedModuleName, range ) ->
                        if String.startsWith moduleName importedModuleName || String.startsWith importedModuleName moduleName then
                            Nothing

                        else if isPrefixedByAnyOf params.allowedModulePrefixes importedModuleName then
                            Nothing

                        else if Set.member importedModuleName seenModules then
                            -- Otherwise, if the imported module is something we've visited
                            -- it means you are importing from this repo; not allowed
                            Just
                                (Review.Rule.errorForModule
                                    moduleKey
                                    { message = "Cannot `import " ++ importedModuleName ++ "` from `module " ++ moduleName ++ "`"
                                    , details = []
                                    }
                                    range
                                )

                        else
                            Nothing
                    )
                    importRanges
            )
        |> List.concat


moduleVisitor : Review.Rule.ModuleRuleSchema {} ModuleContext -> Review.Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Review.Rule.withImportVisitor importVisitor


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node ctx =
    let
        importedModuleName =
            node
                |> Elm.Syntax.Node.value
                |> .moduleName
                |> Elm.Syntax.Node.value
                |> String.join "."
    in
    ( [], { ctx | importRanges = ( importedModuleName, Elm.Syntax.Node.range node ) :: ctx.importRanges } )
