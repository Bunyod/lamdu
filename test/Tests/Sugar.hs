-- | Test sugar convert results (including its actions)

{-# LANGUAGE TypeOperators #-}

module Tests.Sugar where

import qualified Control.Lens as Lens
import qualified Data.List.Class as List
import qualified Data.Property as Property
import           Hyper.Combinator.Ann (Annotated)
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Calc.Term as V
import           Lamdu.Data.Db.Layout (ViewM)
import           Lamdu.Name (Name(..))
import           Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Test.HUnit (assertBool)
import           Test.Lamdu.Env (Env)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "sugar-tests"
    [ delDefParam
    , updateDef
    , delParam
    , delInfixArg
    , paramAnnotations
    , testChangeParam
    , testExtract
    , testExtractForRecursion
    , testLightLambda
    , testNotALightLambda
    , testInline
    , testReorderLets
    , testReplaceParent
    , setHoleToHole
    , testCreateLetInLetVal
    , testFloatToRepl
    , floatLetWithGlobalRef
    , testHoleTypeShown
    , testUnnamed
    , testGroup "insist-tests"
        [ testInsistFactorial
        , testInsistEq
        , testInsistIf
        , testInsistSubsets
        ]
    ]

testSugarActionsWith ::
    HasCallStack =>
    FilePath ->
    [WorkArea Name (T ViewM) (T ViewM)
        (Sugar.Payload Name (T ViewM) (T ViewM) (ParenInfo, [EntityId])) ->
        T ViewM a] ->
    Env ->
    IO ()
testSugarActionsWith program actions env =
    traverse_ (convertWorkArea env >>=) actions <* convertWorkArea env
    & testProgram program

-- | Verify that a sugar action does not result in a crash
testSugarActions ::
    HasCallStack =>
    FilePath ->
    [WorkArea Name (T ViewM) (T ViewM)
        (Sugar.Payload Name (T ViewM) (T ViewM) (ParenInfo, [EntityId])) ->
        T ViewM a] ->
    IO ()
testSugarActions program actions =
    Env.make >>= testSugarActionsWith program actions

replBinder :: Lens.Traversal' (WorkArea name i o a) (Binder name i o # Annotated a)
replBinder = waRepl . replExpr . hVal

replBody :: Lens.Traversal' (WorkArea name i o a) (Term name i o # Annotated a)
replBody = replBinder . _BinderTerm

replLet :: Lens.Traversal' (WorkArea name i o a) (Let name i o # Annotated a)
replLet = replBinder . _BinderLet

lamFirstParam :: Lens.Traversal' (Term name i o a) (FuncParam name i, ParamInfo name i o)
lamFirstParam = _BodyLam . lamFunc . fParams . _Params . Lens.ix 0

testUnnamed :: Test
testUnnamed =
    testSugarActions "unnamed.json" [verify]
    & testCase "name-of-unnamed"
    where
        verify workArea =
            case workArea ^?! replBody . _BodyGetVar . _GetBinder . bvNameRef . nrName of
            Unnamed{} -> pure ()
            _ -> fail "Unexpected name"

-- | Test for issue #374
-- https://trello.com/c/CDLdSlj7/374-changing-tag-results-in-inference-error
testChangeParam :: Test
testChangeParam =
    testSugarActions "apply-id-of-lambda.json" [action]
    & testCase "change-param"
    where
        action workArea =
            workArea ^?!
            replBody . _BodySimpleApply . V.appFunc .
            hVal . _BodySimpleApply . V.appArg .
            hVal . lamFirstParam . _2 . piTag . tagRefReplace . tsNewTag
            >>= (^. toPick)

-- | Test for issue #373
-- https://trello.com/c/1kP4By8j/373-re-ordering-let-items-results-in-inference-error
testReorderLets :: Test
testReorderLets =
    testGroup "reorder-lets"
    [ f "let-items-extract.json"
    , f "let-items-extract-with-tag-clash.json"
    , f "let-items-extract-with-anon-tag.json"
    ]
    where
        f program =
            testSugarActions program [(^?! extractSecondLetItemInLambda)]
            & testCase (takeWhile (/= '.') program)
        extractSecondLetItemInLambda =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . _BinderLet . lBody .
            hVal . _BinderLet . lValue .
            annotation . plActions . extract

-- Test for issue #395
-- https://trello.com/c/UvBdhzzl/395-extract-of-binder-body-with-let-items-may-cause-inference-failure
testExtract :: Test
testExtract =
    testSugarActions "extract-lambda-with-let.json" [(^?! action)]
    & testCase "extract"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody . annotation . plActions .
            extract

-- Test for issue #402
-- https://trello.com/c/ClDnsGQi/402-wrong-result-when-inlining-from-hole-results
testInline :: Test
testInline =
    testSugarActions "let-item-inline.json" [inline, verify]
    & testCase "inline"
    where
        inline workArea =
            do
                yOption <-
                    letItem ^. lBody . hVal . _BinderTerm . _BodyHole
                    . holeOptions
                    >>= findM isY
                    <&> fromMaybe (error "expected option")
                mkResult <-
                    yOption ^. hoResults & List.runList
                    <&>
                    \case
                    List.Cons (_, x) _ -> x
                    List.Nil -> error "expected Cons"
                result <- mkResult
                result ^. holeResultPick
                _ <-
                    result ^?! holeResultConverted . hVal . _BinderTerm
                    . _BodyGetVar . _GetBinder . bvInline . _InlineVar
                pure ()
            where
                letItem =
                    workArea ^?!
                    replBody . _BodyLam . lamFunc . fBody .
                    hVal . _BinderLet
                isY option =
                    option ^. hoSugaredBaseExpr
                    <&> Lens.has
                    (hVal . _BinderTerm . _BodyGetVar . _GetBinder .
                        bvForm . _GetLet)
        verify workArea
            | Lens.has afterInline workArea = pure ()
            | otherwise = fail "Expected inline result"
        afterInline =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyLiteral . _LiteralNum

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM f (x:xs) =
    do
        found <- f x
        if found
            then Just x & pure
            else findM f xs

paramAnnotations :: Test
paramAnnotations =
    Env.make <&> has .~ Annotations.None
    >>= testSugarActionsWith "const-five.json" [verify]
    & testCase "param-annotations"
    where
        verify workArea =
            unless
            (Lens.allOf (replBody . lamFirstParam . _1 . fpAnnotation) (Lens.has _AnnotationNone) workArea)
            (fail "parameter should not have type annotation")

delParam :: Test
delParam =
    testSugarActions "const-five.json" [(^?! action), verify]
    & testCase "del-param"
    where
        action = replBody . lamFirstParam . _2 . piActions . fpDelete
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = fail "Expected 5"
        afterDel = replBody . _BodyLiteral . _LiteralNum

delInfixArg :: Test
delInfixArg =
    testSugarActions "one-plus-one.json" [argDel, holeDel, verify]
    & testCase "del-infix-arg"
    where
        argDel workArea =
            workArea ^?! arg . annotation . plActions . mSetToHole . Lens._Just & void
        holeDel workArea =
            workArea ^?! arg . hVal . _BodyHole . holeMDelete . Lens._Just & void
        arg = replBody . _BodyLabeledApply . aSpecialArgs . _Operator . _2
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = fail "Expected 1"
        afterDel = replBody . _BodyLiteral . _LiteralNum

testExtractForRecursion :: Test
testExtractForRecursion =
    testSugarActions "fold.json"
    [ void . (^?! openDef)
    , void . (^?! extractDef)
    ]
    & testCase "no-extract-recursive"
    where
        openDef =
            replBody . _BodyLabeledApply . aFunc .
            hVal . Lens._Wrapped . bvNameRef . nrGotoDefinition
        extractDef =
            waPanes . traverse . paneBody . _PaneDefinition .
            drBody . _DefinitionBodyExpression . deContent .
            annotation . plActions . extract

testInsistFactorial :: Test
testInsistFactorial =
    testSugarActions "factorial-mismatch.json"
    [ void . (^?! openDef)
    , void . (^?! insist)
    , verify
    ]
    & testCase "insist-factorial"
    where
        openDef =
            replBody . _BodySimpleApply . appFunc .
            hVal . _BodyGetVar . _GetBinder . bvNameRef . nrGotoDefinition
        ifElse =
            waPanes . traverse . paneBody . _PaneDefinition .
            drBody . _DefinitionBodyExpression . deContent .
            hVal . _BodyFunction . fBody .
            hVal . _BinderTerm . _BodyIfElse
        insist =
            Lens.cloneTraversal ifElse . iThen .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyFragment . fHeal
        verify workArea
            | Lens.has unexpected workArea = fail "fragment created at unexpected position"
            | otherwise = pure ()
        unexpected =
            Lens.cloneTraversal ifElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodySimpleApply . appFunc .
            hVal . _BodyFragment

testInsistEq :: Test
testInsistEq =
    testSugarActions "compare-int-and-text.json"
    [ void . (^?! insist)
    , verify
    ]
    & testCase "insist-eq"
    where
        insist =
            replBody . _BodyLabeledApply . aSpecialArgs . _Operator . _2 .
            hVal . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = fail "fragment not created at expected position"
        expected =
            replBody . _BodyLabeledApply . aSpecialArgs . _Operator . _1 .
            hVal . _BodyFragment

testInsistIf :: Test
testInsistIf =
    testSugarActions "if-with-mismatch.json"
    [ void . (^?! insist)
    , verify
    ]
    & testCase "insist-if"
    where
        insist =
            replBody . _BodyIfElse . iThen .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = fail "fragment not created at expected position"
        expected =
            replBody . _BodyIfElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyFragment

testInsistSubsets :: Test
testInsistSubsets =
    testSugarActions "subsets.json"
    [ void . (^?! openDef)
    , void . (^?! insist)
    , verify
    ]
    & testCase "insist-subsets"
    where
        openDef = replBody . _BodyGetVar . _GetBinder . bvNameRef . nrGotoDefinition
        consArgs =
            waPanes . traverse . paneBody . _PaneDefinition .
            drBody . _DefinitionBodyExpression . deContent .
            hVal . _BodyFunction . fBody .
            hVal . _BinderTerm . _BodyCase . cBody . cItems . Lens.ix 1 . ciExpr .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyLabeledApply . aSpecialArgs . _Operator
        insist =
            Lens.cloneTraversal consArgs . Lens._2 .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderLet . lBody .
            hVal . _BinderTerm . _BodyLabeledApply . aSpecialArgs . _Operator . Lens._2 .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyLabeledApply . aSpecialArgs . _Operator . Lens._1 .
            hVal . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = fail "fragment not created at expected position"
        expected = Lens.cloneTraversal consArgs . Lens._1 . hVal . _BodyFragment

testLightLambda :: Test
testLightLambda =
    testSugarActions "fold.json" [verify]
    & testCase "light-lambda"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = fail "Expected light lambda sugar!"
        expected =
            replBody . _BodyLabeledApply . aAnnotatedArgs . traverse . aaExpr .
            hVal . _BodyLam . lamMode . _LightLambda

testNotALightLambda :: Test
testNotALightLambda =
    testSugarActions "lam-in-lam.json" [verify]
    & testCase "not-a-light-lambda"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = fail "Expected light lambda sugar!"
        expected = replBody . _BodyLam . lamMode . _NormalBinder

delDefParam :: Test
delDefParam =
    testSugarActions "def-with-params.json" [void . (^?! openDef), (^?! action)]
    & testCase "del-def-param"
    where
        openDef = replBody . _BodyGetVar . _GetBinder . bvNameRef . nrGotoDefinition
        action =
            waPanes . traverse . paneBody . _PaneDefinition .
            drBody . _DefinitionBodyExpression . deContent .
            hVal . _BodyFunction .
            fParams . _Params . traverse .
            _2 . piActions . fpDelete

updateDef :: Test
updateDef =
    testSugarActions "update-def-type.json" [void . (^?! openDef), void . (^?! action)]
    & testCase "update-def-type"
    where
        openDef = replBody . _BodyGetVar . _GetBinder . bvNameRef . nrGotoDefinition
        action =
            waPanes . traverse . paneBody . _PaneDefinition .
            drBody . _DefinitionBodyExpression . deContent .
            hVal . _BodyFunction . fBody .
            hVal . _BinderTerm . _BodyLabeledApply . aFunc .
            hVal . Lens._Wrapped . bvForm . _GetDefinition . _DefTypeChanged . defTypeUseCurrent

testReplaceParent :: Test
testReplaceParent =
    testSugarActions "let-item-inline.json" [(^?! action)]
    & testCase "replace-parent"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody .
            annotation . plActions . mReplaceParent . Lens._Just

floatLetWithGlobalRef :: Test
floatLetWithGlobalRef =
    testSugarActions "let-with-global-reference.json"
    [ (^?! replLet . lBody . hVal . _BinderLet . lValue . annotation . plActions . extract)
    ]
    & testCase "float-let-with-global-ref"

setHoleToHole :: Test
setHoleToHole =
    testSugarActions "let-item-inline.json" [action, verify]
    & testCase "set-hole-to-hole"
    where
        action workArea = workArea ^?! setToHole & void
        verify workArea
            | Lens.has setToHole workArea =
                fail "hole has set to hole?"
            | otherwise = pure ()
        setToHole :: Lens.Traversal' (WorkArea name i o (Payload name i o a)) (o EntityId)
        setToHole =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . _BinderLet . lValue .
            annotation . plActions . mSetToHole . Lens._Just

assertEq :: (Monad m, Show a, Eq a) => String -> a -> a -> m ()
assertEq msg expected got
    | expected == got = pure ()
    | otherwise =
          "Assertion failed: " ++ msg ++
          "\n  expected to be: " ++ show expected ++
          "\n  but was:        " ++ show got
          & fail

testFloatToRepl :: Test
testFloatToRepl =
    testCase "float-to-repl" $
    do
        env <- Env.make
        testProgram "repl-2-lets.json" $
            do
                workArea <- convertWorkArea env
                assertLetVals workArea 1 2
                void $ workArea ^?! innerLet . annotation . plActions . extract
                newWorkArea <- convertWorkArea env
                assertLetVals newWorkArea 2 1
    where
        assertLetVals workArea outer inner =
            do
                assertEq "Outer let hVal" outer
                    (workArea ^?! replLet . lValue . literalVal)
                assertEq "Inner let hVal" inner
                    (workArea ^?! innerLet . literalVal)

        innerLet ::
            Lens.Traversal' (WorkArea name i o a) (Annotated a # Assignment name i o)
        innerLet = replLet . lBody . hVal . _BinderLet . lValue
        literalVal = hVal . _BodyPlain . apBody . _BinderTerm . _BodyLiteral . _LiteralNum . Property.pVal

testCreateLetInLetVal :: Test
testCreateLetInLetVal =
    testCase "create-let-in-let-hVal" $
    do
        env <- Env.make
        result <-
            testProgram "let-item-inline.json" $
                do
                    workArea <- convertWorkArea env
                    _ <-
                        workArea ^?!
                        theLet . lValue . annotation . plActions . mNewLet .
                        Lens._Just
                    newWorkArea <- convertWorkArea env
                    Lens.has
                        ( theLet . lValue . hVal . _BodyPlain
                        . apBody . _BinderLet
                        ) newWorkArea & pure
        assertBool "Let was not created inside the let-value" result
    where
        -- | Extract from:
        -- >>> \x -> let y = 0 in <hole>
        --           ^^^^^^^^^^^^^^^^^^^
        theLet ::
            Lens.Traversal'
            (WorkArea name i o a)
            (Let name i o # Annotated a)
        theLet = replBody . _BodyLam . lamFunc . fBody . hVal . _BinderLet

testHoleTypeShown :: Test
testHoleTypeShown =
    testCase "hole-type-shown" $
    do
        env <- Env.make <&> has .~ Annotations.None
        workArea <- testProgram "to-nom.json" (convertWorkArea env)
        let x = workArea ^?! replBody . _BodyToNom . nVal
        putStrLn $ case x ^. annotation . plAnnotation of
            AnnotationType {} -> "Type"
            AnnotationVal {} -> "Val"
            AnnotationNone {} -> "None"
        x ^. annotation . plAnnotation
            & Lens.has _AnnotationType
            & assertBool "Expected to have type"
