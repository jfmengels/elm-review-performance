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



-- TODO Report lazy being used on its own?
-- TODO Support lazy used with <| and |>


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
    , currentFunctionHasNoArguments : Bool
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , topLevelFunctionNames = Set.empty
            , currentFunctionHasNoArguments = False
            }
        )
        |> Rule.withModuleNameLookupTable


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor declarations context =
    let
        newContext : Context
        newContext =
            { context | topLevelFunctionNames = Set.fromList (List.filterMap topLevelFunctionNames declarations) }
    in
    ( [], newContext )


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
            , { context | currentFunctionHasNoArguments = hasNoArguments }
            )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue _ functionName)) :: lazyFunctionArgument :: _) ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable functionRange of
                Just moduleName ->
                    if Set.member moduleName lazyModuleNames && Set.member functionName lazyFunctionNames then
                        ( reportUnstableFunctionReference context functionRange lazyFunctionArgument, context )

                    else
                        ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


reportUnstableFunctionReference : Context -> Range -> Node Expression -> List (Rule.Error {})
reportUnstableFunctionReference context functionRange lazyFunctionArgument =
    if context.currentFunctionHasNoArguments || isStableReference context lazyFunctionArgument then
        []

    else
        [ Rule.error
            { message = "Misuse of a lazy function"
            , details = [ "The argument passed to the lazy function must be a stable reference, but a new reference will be created everytime this function is called." ]
            }
            functionRange
        ]


lazyModuleNames : Set ModuleName
lazyModuleNames =
    Set.fromList
        [ -- https://package.elm-lang.org/packages/elm/html/latest/Html.Lazy
          [ "Html", "Lazy" ]
        , -- https://package.elm-lang.org/packages/elm/svg/latest/Svg.Lazy
          [ "Svg", "Lazy" ]
        , -- https://package.elm-lang.org/packages/elm/virtual-dom/latest/VirtualDom
          [ "VirtualDom" ]
        , -- https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Lazy
          [ "Html", "Styled", "Lazy" ]
        , -- https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Svg-Styled-Lazy
          [ "Svg", "Styled", "Lazy" ]
        , -- https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Lazy
          [ "Element", "Lazy" ]
        , -- https://package.elm-lang.org/packages/miniBill/elm-ui-with-context/latest/Element-WithContext-Lazy
          [ "Element", "WithContext", "Lazy" ]
        , -- https://package.elm-lang.org/packages/austinshenk/elm-w3/latest/W3-Html
          [ "W3", "Html" ]
        , -- https://package.elm-lang.org/packages/zwilias/elm-html-string/latest/Html-String-Lazy
          [ "Html", "String", "Lazy" ]
        ]


lazyFunctionNames : Set String
lazyFunctionNames =
    Set.fromList [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7", "lazy8" ]


isStableReference : Context -> Node Expression -> Bool
isStableReference context node =
    case Node.value node of
        Expression.FunctionOrValue moduleName name ->
            isVariableFromLetDeclaration context (Node.range node) moduleName name

        -- TODO Support parens
        _ ->
            False


isVariableFromLetDeclaration : Context -> Range -> ModuleName -> String -> Bool
isVariableFromLetDeclaration context range moduleName name =
    if moduleName == [] then
        Set.member name context.topLevelFunctionNames

    else
        ModuleNameLookupTable.moduleNameAt context.lookupTable range /= Just []
