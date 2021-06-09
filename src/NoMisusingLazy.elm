module NoMisusingLazy exposing
    ( rule
    , Configuration, defaults, withLazyModules
    )

{-|

@docs rule

-}

import Elm.Module
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
-- TODO Add configuration to specify whether to consider functions as creating new references by default or not, and taking a list of exceptions
-- TODO Consider `Html.Lazy.lazy` created with pipes
-- TODO Report about unnecessary lazy, when view functions are called with data not from the arguments, and can be extracted to a top-level constant. Maybe behind a flag?


{-| Reports... REPLACEME

    config =
        [ NoMisusingLazy.rule NoMisusingLazy.defaults
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
rule : Configuration -> Rule
rule (Configuration { lazyModules }) =
    let
        parsedModuleNames : Result (List String) (List ModuleName)
        parsedModuleNames =
            lazyModules
                |> List.map parseModuleNames
                |> resultSequence (Ok [])
    in
    case parsedModuleNames of
        Ok configurationLazyModulesNames ->
            let
                lazyModuleNames : Set ModuleName
                lazyModuleNames =
                    Set.union baseLazyModuleNames (Set.fromList configurationLazyModulesNames)
            in
            Rule.newModuleRuleSchemaUsingContextCreator "NoMisusingLazy" initialContext
                |> Rule.withDeclarationListVisitor (declarationListVisitor lazyModuleNames)
                |> Rule.withDeclarationEnterVisitor declarationVisitor
                |> Rule.withExpressionExitVisitor (expressionVisitor lazyModuleNames)
                |> Rule.fromModuleRuleSchema

        Err errors ->
            Rule.configurationError "NoMisusingLazy"
                { message = "I found some problems with the arguments to withLazyModules"
                , details =
                    [ errors
                        |> List.map (\str -> "  - " ++ str)
                        |> List.reverse
                        |> String.join "\n"
                    ]
                }


parseModuleNames : String -> Result String (List String)
parseModuleNames moduleName =
    if moduleName == "" then
        Err "One of the module names I received was empty"

    else
        case Elm.Module.fromString moduleName of
            Just _ ->
                Ok (String.split "." moduleName)

            Nothing ->
                Err (moduleName ++ " is not a valid module name")


resultSequence : Result (List x) (List a) -> List (Result x a) -> Result (List x) (List a)
resultSequence acc results =
    case results of
        [] ->
            acc

        (Ok ok) :: restOfResults ->
            case acc of
                Ok previousOks ->
                    resultSequence (Ok (ok :: previousOks)) restOfResults

                Err _ ->
                    acc

        (Err err) :: restOfResults ->
            case acc of
                Err previousErrors ->
                    resultSequence (Err (err :: previousErrors)) restOfResults

                Ok _ ->
                    resultSequence (Err [ err ]) restOfResults



-- CONFIGURATION


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`withLazyModules`](#withLazyModules) to alter it.
-}
type Configuration
    = Configuration
        { lazyModules : List String
        }


{-| Default configuration for this rule. Use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) if you want to change the configuration.

    config =
        [ Simplify.defaults
            |> Simplify.withLazyModules [ "Some.Module.Name" ]
            |> Simplify.rule
        ]

-}
defaults : Configuration
defaults =
    Configuration { lazyModules = [] }


withLazyModules : List String -> Configuration -> Configuration
withLazyModules lazyModules (Configuration config) =
    Configuration { config | lazyModules = lazyModules ++ config.lazyModules }


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , topLevelFunctionNames : Set String
    , currentFunctionHasNoArguments : Bool
    , lazyFunctions : Set ( ModuleName, String )
    , coreLazyFunctionReferences : List Range
    , lazyFunctionReferences : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , topLevelFunctionNames = Set.empty
            , currentFunctionHasNoArguments = False
            , lazyFunctions = Set.empty
            , coreLazyFunctionReferences = []
            , lazyFunctionReferences = []
            }
        )
        |> Rule.withModuleNameLookupTable


declarationListVisitor : Set ModuleName -> List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor lazyModuleNames declarations context =
    let
        newContext : Context
        newContext =
            { context | topLevelFunctionNames = Set.fromList (List.filterMap topLevelFunctionNames declarations) }

        lazyFunctions : Set ( ModuleName, String )
        lazyFunctions =
            declarations
                |> List.filterMap (checkIfIsLazyFunction lazyModuleNames context.lookupTable)
                |> List.map (Tuple.pair [])
                |> Set.fromList
    in
    ( [], { newContext | lazyFunctions = Set.union lazyFunctions newContext.lazyFunctions } )


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


checkIfIsLazyFunction : Set ModuleName -> ModuleNameLookupTable -> Node Declaration -> Maybe String
checkIfIsLazyFunction lazyModuleNames lookupTable node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if checkIfReturnsLazyFunction lazyModuleNames lookupTable declaration.expression then
                Just (Node.value declaration.name)

            else
                Nothing

        _ ->
            Nothing


checkIfReturnsLazyFunction : Set ModuleName -> ModuleNameLookupTable -> Node Expression -> Bool
checkIfReturnsLazyFunction lazyModuleNames lookupTable node =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue _ functionName)) :: _ :: _) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable functionRange of
                Just moduleName ->
                    Set.member moduleName lazyModuleNames && Set.member functionName lazyFunctionNames

                Nothing ->
                    False

        Expression.OperatorApplication _ _ _ _ ->
            -- TODO
            False

        Expression.ParenthesizedExpression expr ->
            checkIfReturnsLazyFunction lazyModuleNames lookupTable expr

        Expression.LetExpression { expression } ->
            checkIfReturnsLazyFunction lazyModuleNames lookupTable expression

        Expression.IfBlock _ _ _ ->
            -- ???
            False

        Expression.CaseExpression _ ->
            -- ???
            False

        Expression.LambdaExpression _ ->
            -- ???
            False

        _ ->
            False


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


expressionVisitor : Set ModuleName -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor lazyModuleNames node context =
    case Node.value node of
        Expression.FunctionOrValue _ functionName ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                Just moduleName ->
                    if Set.member moduleName lazyModuleNames && Set.member functionName lazyFunctionNames then
                        ( []
                        , { context | coreLazyFunctionReferences = Node.range node :: context.coreLazyFunctionReferences }
                        )

                    else if Set.member ( moduleName, functionName ) context.lazyFunctions then
                        ( []
                        , { context | lazyFunctionReferences = Node.range node :: context.lazyFunctionReferences }
                        )

                    else
                        ( [], context )

                Nothing ->
                    ( [], context )

        Expression.ParenthesizedExpression expr ->
            if List.member (Node.range expr) context.coreLazyFunctionReferences then
                ( [], { context | coreLazyFunctionReferences = Node.range node :: context.coreLazyFunctionReferences } )

            else if List.member (Node.range expr) context.lazyFunctionReferences then
                ( [], { context | lazyFunctionReferences = Node.range node :: context.lazyFunctionReferences } )

            else
                ( [], context )

        Expression.Application (function :: firstArgument :: restOfArguments) ->
            handleFunctionCall (Node.range node) function firstArgument restOfArguments context

        Expression.OperatorApplication "<|" _ left right ->
            handleFunctionCall (Node.range node) left right [] context

        Expression.OperatorApplication "|>" _ left right ->
            handleFunctionCall (Node.range node) right left [] context

        _ ->
            ( [], context )


handleFunctionCall : Range -> Node b -> Node Expression -> List (Node Expression) -> Context -> ( List (Rule.Error {}), Context )
handleFunctionCall range function firstArgument restOfArguments context =
    if List.member (Node.range function) context.coreLazyFunctionReferences then
        ( reportUnstableFunctionReference context firstArgument ++ reportUnstableArgumentReferences context restOfArguments
        , { context | lazyFunctionReferences = range :: context.lazyFunctionReferences }
        )

    else if List.member (Node.range function) context.lazyFunctionReferences then
        ( reportUnstableArgumentReferences context (firstArgument :: restOfArguments)
        , { context | lazyFunctionReferences = range :: context.lazyFunctionReferences }
        )

    else
        ( [], context )


reportUnstableFunctionReference : Context -> Node Expression -> List (Rule.Error {})
reportUnstableFunctionReference context lazyFunctionArgument =
    if context.currentFunctionHasNoArguments || isStableReference context lazyFunctionArgument then
        []

    else
        [ Rule.error
            { message = "Misuse of a lazy function"
            , details = [ "The argument passed to the lazy function must be a stable reference, but a new reference will be created everytime this function is called." ]
            }
            (Node.range lazyFunctionArgument)
        ]


reportUnstableArgumentReferences : Context -> List (Node Expression) -> List (Rule.Error {})
reportUnstableArgumentReferences context arguments =
    if not context.currentFunctionHasNoArguments then
        arguments
            |> List.filter isArgumentANewReference
            |> List.map
                (\arg ->
                    Rule.error
                        { message = "Argument de-optimizes a lazy function"
                        , details =
                            [ "This argument is passed to a function that is being optimized by a 'lazy' function."
                            , "The way this argument is constructed creates a new reference every time, which invalidates the lazy function's optimization."
                            ]
                        }
                        (Node.range arg)
                )

    else
        []


isArgumentANewReference : Node Expression -> Bool
isArgumentANewReference node =
    case Node.value node of
        Expression.RecordExpr _ ->
            True

        Expression.RecordUpdateExpression _ _ ->
            True

        Expression.TupledExpression _ ->
            True

        Expression.ListExpr _ ->
            True

        Expression.RecordAccessFunction _ ->
            True

        Expression.LambdaExpression _ ->
            True

        Expression.Application ((Node _ (Expression.FunctionOrValue _ functionName)) :: _) ->
            isTypeOrTypeAliasConstructor functionName

        Expression.OperatorApplication "<|" _ left _ ->
            isFunctionCallOfTypeConstructor left

        Expression.OperatorApplication "|>" _ _ right ->
            isFunctionCallOfTypeConstructor right

        Expression.ParenthesizedExpression expr ->
            isArgumentANewReference expr

        _ ->
            False


isFunctionCallOfTypeConstructor : Node Expression -> Bool
isFunctionCallOfTypeConstructor node =
    case Node.value node of
        Expression.FunctionOrValue _ functionName ->
            isTypeOrTypeAliasConstructor functionName

        Expression.Application ((Node _ (Expression.FunctionOrValue _ functionName)) :: _) ->
            isTypeOrTypeAliasConstructor functionName

        Expression.ParenthesizedExpression expr ->
            isFunctionCallOfTypeConstructor expr

        _ ->
            False


isTypeOrTypeAliasConstructor : String -> Bool
isTypeOrTypeAliasConstructor functionName =
    case String.uncons functionName of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


baseLazyModuleNames : Set ModuleName
baseLazyModuleNames =
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
        ]


lazyFunctionNames : Set String
lazyFunctionNames =
    Set.fromList [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5", "lazy6", "lazy7", "lazy8" ]


isStableReference : Context -> Node Expression -> Bool
isStableReference context node =
    case Node.value node of
        Expression.FunctionOrValue moduleName name ->
            isVariableFromLetDeclaration context (Node.range node) moduleName name

        Expression.ParenthesizedExpression expr ->
            isStableReference context expr

        _ ->
            False


isVariableFromLetDeclaration : Context -> Range -> ModuleName -> String -> Bool
isVariableFromLetDeclaration context range moduleName name =
    if moduleName == [] then
        Set.member name context.topLevelFunctionNames

    else
        ModuleNameLookupTable.moduleNameAt context.lookupTable range /= Just []
