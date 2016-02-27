{-# LANGUAGE NoImplicitPrelude, RankNTypes, LambdaCase #-}
module Lamdu.EvalManager
    ( Evaluator
    , new
    , start, stop
    , getResults
    , runTransactionAndMaybeRestartEvaluator
    ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (runAfter)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (when)
import           Data.Binary.Utils (decodeS)
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Foldable (traverse_)
import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import qualified Data.Monoid as Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Db (Db)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins as Builtins
import qualified Lamdu.Compiler.Javascript as Compiler
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (DbM, ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.Background as EvalBG
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Val as V
import           Lamdu.VersionControl (getVersion)
import qualified Lamdu.VersionControl as VersionControl
import qualified System.IO as IO

import           Prelude.Compat

type T = Transaction

data BGEvaluator = NotStarted | Started (EvalBG.Evaluator (ValI ViewM))

startedEvaluator :: BGEvaluator -> Maybe (EvalBG.Evaluator (ValI ViewM))
startedEvaluator NotStarted = Nothing
startedEvaluator (Started eval) = Just eval

data Evaluator = Evaluator
    { eInvalidateCache :: IO ()
    , eDb :: MVar (Maybe Db)
    , eEvaluatorRef :: IORef BGEvaluator
    , eResultsRef :: IORef (EvalResults (ValI ViewM))
    , eCancelTimerRef :: IORef (Maybe ThreadId)
    }

new :: IO () -> MVar (Maybe Db) -> IO Evaluator
new invalidateCache db =
    do
        ref <- newIORef NotStarted
        resultsRef <- newIORef EvalResults.empty
        cancelRef <- newIORef Nothing
        return Evaluator
            { eInvalidateCache = invalidateCache
            , eDb = db
            , eEvaluatorRef = ref
            , eResultsRef = resultsRef
            , eCancelTimerRef = cancelRef
            }

withDb :: MVar (Maybe Db) -> (Db -> IO a) -> IO a
withDb mvar action =
    withMVar mvar $ \case
    Nothing -> error "Trying to use DB when it is already gone"
    Just db -> action db

runViewTransactionInIO :: MVar (Maybe Db) -> T ViewM a -> IO a
runViewTransactionInIO dbMVar trans =
    withDb dbMVar $ \db ->
    DbLayout.runDbTransaction db (VersionControl.runAction trans)

getLatestResults :: Evaluator -> IO (EvalResults (ValI ViewM))
getLatestResults evaluator =
    readIORef (eEvaluatorRef evaluator) <&> startedEvaluator
    >>= maybe (return EvalResults.empty) EvalBG.getResults

getResults :: Evaluator -> IO (CurAndPrev (EvalResults (ValI ViewM)))
getResults evaluator =
    do
        res <- getLatestResults evaluator
        prevResults <- readIORef (eResultsRef evaluator)
        return CurAndPrev { _prev = prevResults, _current = res }

loadDef ::
    Evaluator -> DefI ViewM ->
    IO (Def.Definition (V.Val (ExprIRef.ValIProperty ViewM)) (DefI ViewM))
loadDef evaluator = runViewTransactionInIO (eDb evaluator) . Load.loadDef

evalActions :: Evaluator -> EvalBG.Actions (ValI ViewM)
evalActions evaluator =
    EvalBG.Actions
    { EvalBG._aLoadGlobal = loadGlobal
    , EvalBG._aRunBuiltin = Builtins.eval
    , EvalBG._aReportUpdatesAvailable = eInvalidateCache evaluator
    , EvalBG._aCompleted = \_ ->
          do
              atomicModifyIORef_ (eResultsRef evaluator) (const EvalResults.empty)
              eInvalidateCache evaluator
    }
    where
        loadGlobal globalId =
            ExprIRef.defI globalId
            & loadDef evaluator
            <&> asDef
        asDef x =
            x ^. Def.defBody
            <&> Lens.mapped %~ Property.value
            & Just

replIRef :: IRef ViewM (ValI ViewM)
replIRef = DbLayout.repl DbLayout.codeIRefs

start :: Evaluator -> IO ()
start evaluator =
    Transaction.readIRef replIRef >>= ExprIRef.readVal
    & runViewTransactionInIO (eDb evaluator)
    >>= EvalBG.start (evalActions evaluator) <&> Started
    >>= writeIORef (eEvaluatorRef evaluator)

stop :: Evaluator -> IO ()
stop evaluator =
    do
        readIORef (eEvaluatorRef evaluator)
            <&> startedEvaluator
            >>= traverse_ EvalBG.stop
        writeIORef (eEvaluatorRef evaluator) NotStarted
        writeIORef (eResultsRef evaluator) EvalResults.empty

sumDependency :: Set (ExprIRef.ValI ViewM) -> Set V.Var -> Set Guid
sumDependency subexprs globals =
    mconcat
    [ Set.map (IRef.guid . ExprIRef.unValI) subexprs
    , Set.map (IRef.guid . ExprIRef.defI) globals
    ]

guidToInt :: Guid -> Int
guidToInt = decodeS . Guid.bs

valId :: ValI m -> Compiler.ValId
valId = Compiler.ValId . guidToInt . IRef.guid . ExprIRef.unValI

readVal :: Monad m => ValI m -> T m (V.Val Compiler.ValId)
readVal = (fmap . fmap) valId . ExprIRef.readVal

compileRepl :: (forall a. T DbM a -> IO a) -> IO ()
compileRepl runTrans =
    IO.withFile "output.js" IO.WriteMode $
    \outFile ->
    do
        let actions =
                Compiler.Actions
                { Compiler.readAssocName =
                  runViewTransaction . Transaction.getP . Anchors.assocNameRef
                , Compiler.readGlobal =
                  \globalId ->
                  do
                      defBody <- ExprIRef.defI globalId & Transaction.readIRef
                      defBody
                          & traverse readVal
                  & runViewTransaction
                , Compiler.output = IO.hPutStrLn outFile
                }
        replVal <-
            Transaction.readIRef replIRef
            >>= readVal
            & runViewTransaction
        Compiler.compile actions replVal
    where
        runViewTransaction :: T ViewM a -> IO a
        runViewTransaction = runTrans . VersionControl.runAction

runTransactionAndMaybeRestartEvaluator :: Evaluator -> T DbM a -> IO a
runTransactionAndMaybeRestartEvaluator evaluator transaction =
    readIORef (eEvaluatorRef evaluator)
    >>= \case
    NotStarted -> runTrans transaction
    Started eval ->
        do
            dependencies <-
                EvalBG.pauseLoading eval
                <&> uncurry sumDependency
                <&> Set.insert (IRef.guid replIRef)
            (haveNewVersion, dependencyChanged, result) <-
                do
                    (oldVersion, result, newVersion) <-
                        (,,) <$> getVersion <*> transaction <*> getVersion
                    let checkDependencyChange versionData =
                            Version.changes versionData
                            <&> Change.objectKey <&> (`Set.member` dependencies)
                            <&> Monoid.Any & mconcat & return
                    Monoid.Any dependencyChanged <-
                        Version.walk checkDependencyChange checkDependencyChange oldVersion newVersion
                    return (oldVersion /= newVersion, dependencyChanged, result)
                & runTrans
            if dependencyChanged
                then do
                    prevResults <- getLatestResults evaluator
                    stop evaluator
                    setCancelTimer evaluator
                    atomicModifyIORef_ (eResultsRef evaluator) (const prevResults)
                    start evaluator
                else EvalBG.resumeLoading eval
            when haveNewVersion $ compileRepl runTrans
            return result
    where
        runTrans trans =
            withDb (eDb evaluator) $ \db -> DbLayout.runDbTransaction db trans

setCancelTimer :: Evaluator -> IO ()
setCancelTimer evaluator =
    do
        newCancelTimer <- runAfter 5000000 -- 5 seconds
            (atomicModifyIORef (eResultsRef evaluator)
                (flip (,) () . const EvalResults.empty)
            )
        atomicModifyIORef (eCancelTimerRef evaluator)
            (\x -> (Just newCancelTimer, x))
            >>= Lens.traverseOf_ Lens._Just killThread
