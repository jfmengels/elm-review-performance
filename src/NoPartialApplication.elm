module NoPartialApplication exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoPartialApplication.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example --rules NoPartialApplication
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoPartialApplication" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor


type alias ProjectContext =
    {}


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , functionArity : FunctionArityDict
    , nodesToIgnore : List Range
    }


type alias FunctionArityDict =
    Dict ModuleName (Dict String Int)


initialProjectContext : {}
initialProjectContext =
    {}


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable _ ->
            { lookupTable = lookupTable
            , functionArity = Dict.empty
            , nodesToIgnore = []
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\_ -> {})


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts _ previousContext =
    previousContext


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    let
        functionArity : Dict String Int
        functionArity =
            List.foldl inferArityForDeclaration Dict.empty declarations
    in
    ( [], { context | functionArity = Dict.insert [] functionArity context.functionArity } )


inferArityForDeclaration : Node Declaration -> Dict String Int -> Dict String Int
inferArityForDeclaration node dict =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            inferArityForFunction (Node.value declaration) dict

        _ ->
            dict


inferArityForFunction : Expression.FunctionImplementation -> Dict String Int -> Dict String Int
inferArityForFunction function dict =
    if List.isEmpty function.arguments then
        dict

    else
        Dict.insert (Node.value function.name) (List.length function.arguments) dict


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor _ context =
    ( [], { context | nodesToIgnore = [] } )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    if List.member (Node.range node) context.nodesToIgnore then
        ( [], context )

    else
        expressionVisitorHelp node context


expressionVisitorHelp : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitorHelp node context =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue [] name)) :: arguments) ->
            ( report context name functionRange (List.length arguments), context )

        Expression.OperatorApplication "|>" _ _ right ->
            handlePipeline context right

        Expression.OperatorApplication "<|" _ left _ ->
            handlePipeline context left

        _ ->
            ( [], context )


handlePipeline : ModuleContext -> Node Expression -> ( List (Rule.Error {}), ModuleContext )
handlePipeline context node =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue [] name)) :: arguments) ->
            ( report context name functionRange (List.length arguments + 1)
            , { context | nodesToIgnore = Node.range node :: context.nodesToIgnore }
            )

        Expression.FunctionOrValue [] name ->
            ( report context name (Node.range node) 1
            , { context | nodesToIgnore = Node.range node :: context.nodesToIgnore }
            )

        _ ->
            ( [], context )


report : ModuleContext -> String -> Range -> Int -> List (Rule.Error {})
report context name functionRange nbArguments =
    case getArity context name functionRange of
        Just expectedNbArguments ->
            if nbArguments < expectedNbArguments then
                [ Rule.error
                    { message = "REPLACEME"
                    , details = [ "REPLACEME" ]
                    }
                    functionRange
                ]

            else
                []

        Nothing ->
            []


getArity : ModuleContext -> String -> Range -> Maybe Int
getArity context name functionRange =
    ModuleNameLookupTable.moduleNameAt context.lookupTable functionRange
        |> Maybe.andThen (\moduleName -> Dict.get moduleName context.functionArity)
        |> Maybe.andThen (\dict -> Dict.get name dict)
