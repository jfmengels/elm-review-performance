module NoMisusingLazy exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)



-- TODO Report lazy being used on its own
-- TODO Handle calls through operators
-- TODO Support Element.lazy and other known lazy functions (https://klaftertief.github.io/elm-search/?q=lazy)


{-| Reports... REPLACEME

    config =
        [ NoMisusingLazy.rule
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
elm-review --template jfmengels/elm-review-performance/example --rules NoMisusingLazy
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoMisusingLazy" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , topLevelFunctionNames : Set String
    , functionHasNoArguments : Bool
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , topLevelFunctionNames = Set.empty
            , functionHasNoArguments = False
            }
        )
        |> Rule.withModuleNameLookupTable


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor declarations context =
    ( [], { context | topLevelFunctionNames = Set.fromList (List.filterMap topLevelFunctionNames declarations) } )


topLevelFunctionNames : Node Declaration -> Maybe String
topLevelFunctionNames node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            function.declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        _ ->
            Nothing


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                hasNoArguments : Bool
                hasNoArguments =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.isEmpty
            in
            ( []
            , { context | functionHasNoArguments = hasNoArguments }
            )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node lazyRange (Expression.FunctionOrValue _ functionName)) :: lazifiedFunction :: _) ->
            if Set.member functionName lazyFunctionNames then
                case ModuleNameLookupTable.moduleNameAt context.lookupTable lazyRange of
                    Just moduleName ->
                        if Set.member moduleName (Set.fromList [ [ "Html", "Lazy" ], [ "Svg", "Lazy" ] ]) then
                            if context.functionHasNoArguments || isStableReference context lazifiedFunction then
                                ( [], context )

                            else
                                ( [ Rule.error
                                        { message = "Misuse of a lazy function"
                                        , details = [ "The argument passed to the lazy function must be a stable reference, but a new reference will be created everytime this function is called." ]
                                        }
                                        lazyRange
                                  ]
                                , context
                                )

                        else
                            ( [], context )

                    _ ->
                        ( [], context )

            else
                ( [], context )

        _ ->
            ( [], context )


lazyFunctionNames : Set String
lazyFunctionNames =
    Set.fromList [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7", "lazy8" ]


isStableReference : Context -> Node Expression -> Bool
isStableReference context node =
    case Node.value node of
        Expression.FunctionOrValue moduleName name ->
            isVariableFromLetDeclaration context (Node.range node) moduleName name

        _ ->
            False


isVariableFromLetDeclaration : Context -> Range -> ModuleName -> String -> Bool
isVariableFromLetDeclaration context range moduleName name =
    if moduleName == [] then
        Set.member name context.topLevelFunctionNames

    else
        ModuleNameLookupTable.moduleNameAt context.lookupTable range /= Just []
