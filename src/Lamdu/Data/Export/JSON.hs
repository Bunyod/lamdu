-- | Import/Export JSON support
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators, FlexibleInstances #-}
module Lamdu.Data.Export.JSON
    ( fileExportRepl, jsonExportRepl
    , fileExportAll, verifyAll
    , fileExportDef, fileExportTag
    , fileImportAll
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (WriterT, runWriterT)
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AesonDiff
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified Data.List as List
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import           Hyper
import           Hyper.Recurse (unwrapM, (##>>))
import           Hyper.Type.Functor (_F)
import           Hyper.Type.Prune (Prune)
import           Lamdu.Calc.Identifier (Identifier)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import           Lamdu.Data.Export.JSON.Codec (Entity(..))
import qualified Lamdu.Data.Export.JSON.Migration as Migration
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Expr.IRef (ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (ToUUID(..))
import           Revision.Deltum.Hyper (writeRecursively, _ExistingRef)
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Visited = Visited
    { _visitedDefs :: Set V.Var
    , _visitedTags :: Set T.Tag
    , _visitedNominals :: Set T.NominalId
    }
Lens.makeLenses ''Visited

type Export m = WriterT [Entity] (StateT Visited (T m))

type EntityOrdering = (Int, Identifier)

entityOrdering :: Entity -> EntityOrdering
entityOrdering (EntitySchemaVersion _)                          = (0, "")
entityOrdering (EntityTag (Codec.TagEntity (T.Tag ident) _))    = (1, ident)
entityOrdering (EntityNominal (Codec.NominalEntity _ (T.NominalId nomId) _)) = (2, nomId)
entityOrdering (EntityLamVar (Codec.LamVarEntity _ (V.Var ident))) = (3, ident)
entityOrdering (EntityDef (Definition _ _ (_, _, V.Var ident))) = (4, ident)
entityOrdering (EntityRepl _)                                   = (5, "")

currentVersion :: Codec.SchemaVersion
currentVersion = Codec.SchemaVersion Migration.currentVersion

entityVersion :: Entity
entityVersion = EntitySchemaVersion currentVersion

runExport :: Monad m => Export m a -> T m (a, Aeson.Value)
runExport act =
    act
    & runWriterT
    <&> _2 %~ Aeson.toJSON . List.sortOn entityOrdering . (entityVersion :)
    & (`State.evalStateT` Visited mempty mempty mempty)

trans :: Monad m => T m a -> Export m a
trans = lift . lift

withVisited ::
    (Monad m, Ord a) =>
    Lens.ALens' Visited (Set a) -> a -> Export m () -> Export m ()
withVisited l x act =
    do
        alreadyVisited <- Lens.use (Lens.cloneLens l . Lens.contains x)
        unless alreadyVisited $
            do
                Lens.assign (Lens.cloneLens l . Lens.contains x) True
                act

readAssocTag :: Monad m => ToUUID a => a -> T m T.Tag
readAssocTag = Property.getP . Anchors.assocTag

tell :: Monad m => Entity -> Export m ()
tell = Writer.tell . (: [])

exportTag :: Monad m => T.Tag -> Export m ()
exportTag tag
    | tag == Anchors.anonTag = pure ()
    | otherwise =
        ExprIRef.readTagData tag & trans
        >>= tell . EntityTag . Codec.TagEntity tag
        & withVisited visitedTags tag

exportNominal :: Monad m => T.NominalId -> Export m ()
exportNominal nomId =
    do
        nominal <- trans (Load.nominal nomId)
        tag <- readAssocTag nomId & trans
        EntityNominal (Codec.NominalEntity tag nomId nominal) & tell
        & withVisited visitedNominals nomId

class ExportSubexpr k where
    exportSubexpr :: Monad m => Ann (HRef m) # k -> Export m ()
    exportSubexpr _ = pure ()

instance ExportSubexpr V.Term where
    exportSubexpr (Ann _ (V.BLam (V.TypedLam lamVar _ _))) =
        do
            tag <- readAssocTag lamVar & trans
            exportTag tag
            EntityLamVar (Codec.LamVarEntity tag lamVar) & tell
    exportSubexpr _ = pure ()

instance ExportSubexpr (HCompose Prune T.Type)
    -- TODO: Export noms!

instance ExportSubexpr (HCompose Prune T.Row)
    -- TODO: Export used tags!

exportVal :: Monad m => Ann (HRef m) # V.Term -> Export m ()
exportVal x =
    do
        (x & hflipped %~ hmap (const (const (Const ()))))
            ^.. ExprLens.valGlobals mempty
            & traverse_ exportDef
        x ^.. ExprLens.valTags & traverse_ exportTag
        x ^.. ExprLens.valNominals & traverse_ exportNominal
        () <$ unwrapM (Proxy @ExportSubexpr ##>> \n -> n ^. hVal <$ exportSubexpr n) x

exportDef :: Monad m => V.Var -> Export m ()
exportDef globalId =
    do
        presentationMode <- Property.getP (Anchors.assocPresentationMode globalId) & trans
        tag <- readAssocTag globalId & trans
        exportTag tag
        def <- Load.def defI & trans
        def ^. Definition.defBody & traverse_ exportVal
        let def' =
                def
                & Definition.defBody . Lens.mapped . hflipped %~
                    hmap (const (Const . toUUID . (^. ExprIRef.iref)))
        (presentationMode, tag, globalId) <$ def' & EntityDef & tell
    & withVisited visitedDefs globalId
    where
        defI = ExprIRef.defI globalId

exportRepl :: Export ViewM ()
exportRepl =
    do
        repl <- Load.defExpr (DbLayout.repl DbLayout.codeAnchors) & trans
        traverse_ exportVal repl
        repl
            <&> hflipped %~ hmap (const (Const . toUUID . (^. ExprIRef.iref)))
            & EntityRepl & tell

jsonExportRepl :: T ViewM Aeson.Value
jsonExportRepl = runExport exportRepl <&> snd

fileExportRepl :: FilePath -> T ViewM (IO ())
fileExportRepl = export "repl" exportRepl

fileExportDef :: Monad m => V.Var -> FilePath -> T m (IO ())
fileExportDef globalId =
    export ("def: " ++ show globalId) (exportDef globalId)

fileExportTag :: Monad m => T.Tag -> FilePath -> T m (IO ())
fileExportTag tag =
    export ("tag: " ++ show tag) (exportTag tag)

exportAll :: Export ViewM ()
exportAll =
    do
        exportSet DbLayout.globals (exportDef . ExprIRef.globalId)
        exportSet DbLayout.tags exportTag
        exportSet DbLayout.tids exportNominal
        exportRepl
    where
        exportSet indexIRef exportFunc =
            indexIRef DbLayout.codeIRefs & Transaction.readIRef & trans
            >>= traverse_ exportFunc

-- | Verify that the data in the database is valid
verifyAll :: T ViewM ()
verifyAll = runExport exportAll & void

fileExportAll :: FilePath -> T ViewM (IO ())
fileExportAll = export "all" exportAll

export :: Monad m => String -> Export m a -> FilePath -> T m (IO ())
export msg act exportPath =
    runExport act
    <&> snd
    <&> \json ->
        do
            putStrLn $ "Exporting " ++ msg ++ " to " ++ show exportPath
            LBS.writeFile exportPath (AesonPretty.encodePretty json)

writeValAtUUID :: Monad m => Val UUID -> T m (ValI m)
writeValAtUUID x =
    x
    & hflipped %~ hmap (const ((:*: Const ()) . (_ExistingRef . _F #) . IRef.unsafeFromUUID . getConst))
    & writeRecursively
    <&> (^. hAnn . _1)

insertTo ::
    (Monad m, Ord a, Binary a) =>
    a -> (DbLayout.Code (IRef ViewM) ViewM -> IRef m (Set a)) -> T m ()
insertTo item setIRef =
    Transaction.readIRef iref
    <&> Set.insert item
    >>= Transaction.writeIRef iref
    where
        iref = setIRef DbLayout.codeIRefs

importDef :: Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var) -> T ViewM ()
importDef (Definition defBody defScheme (presentationMode, tag, globalId)) =
    do
        Property.setP (Anchors.assocPresentationMode globalId) presentationMode
        Property.setP (Anchors.assocTag globalId) tag
        bodyValI <- Lens.traverse writeValAtUUID defBody
        Definition bodyValI defScheme () & Transaction.writeIRef defI
        defI `insertTo` DbLayout.globals
    where
        defI = ExprIRef.defI globalId

importRepl :: Definition.Expr (Val UUID) -> T ViewM ()
importRepl defExpr =
    traverse writeValAtUUID defExpr >>=
    Transaction.writeIRef (DbLayout.repl DbLayout.codeIRefs)

importTag :: Codec.TagEntity -> T ViewM ()
importTag (Codec.TagEntity tagId tagData) =
    do
        Transaction.writeIRef (ExprIRef.tagI tagId) tagData
        tagId `insertTo` DbLayout.tags

importNominal :: Codec.NominalEntity -> T ViewM ()
importNominal (Codec.NominalEntity tag nomId nominal) =
    do
        Property.setP (Anchors.assocTag nomId) tag
        traverse_ (Transaction.writeIRef (ExprIRef.nominalI nomId)) nominal
        nomId `insertTo` DbLayout.tids

importLamVar :: Monad m => Codec.LamVarEntity -> T m ()
importLamVar (Codec.LamVarEntity tag var) = Property.setP (Anchors.assocTag var) tag

importOne :: Entity -> T ViewM ()
importOne (EntityDef x) = importDef x
importOne (EntityRepl x) = importRepl x
importOne (EntityTag x) = importTag x
importOne (EntityNominal x) = importNominal x
importOne (EntityLamVar x) = importLamVar x
importOne (EntitySchemaVersion _) =
    fail "Only one schemaVersion allowed in beginning of document"

importEntities :: [Entity] -> T ViewM ()
importEntities (EntitySchemaVersion ver : entities) =
    if ver == currentVersion
    then traverse_ importOne entities
    else "Unsupported schema version: " ++ show ver & fail
importEntities _ = "Missing schema version"  & fail

fileImportAll :: FilePath -> IO (T ViewM ())
fileImportAll importPath =
    do
        putStrLn $ "importing from: " ++ show importPath
        migrated <-
            LBS.readFile importPath <&> Aeson.eitherDecode
            >>= either fail pure
            >>= Migration.migrateAsNeeded
        case Aeson.fromJSON migrated of
            Aeson.Error str -> fail str
            Aeson.Success entities
                | reencoded == migrated -> importEntities entities & pure
                | otherwise ->
                    "JSON codec ignored fields:\n" <>
                    LBSChar.unpack
                    (AesonPretty.encodePretty (Aeson.toJSON (AesonDiff.diff reencoded migrated)))
                    & fail
                where
                    reencoded = Aeson.toJSON entities
