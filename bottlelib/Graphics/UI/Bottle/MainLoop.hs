{-# LANGUAGE NoImplicitPrelude, CPP, RecordWildCards #-}
module Graphics.UI.Bottle.MainLoop
    ( AnimConfig (..)
    , mainLoopAnim
    , mainLoopImage
    , mainLoopWidget
    ) where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void, when, unless, forever)
import qualified Control.Monad.STM as STM
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Monoid as Monoid
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import           Graphics.DrawingCombinators.Utils (Image)
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Result(..), eventLoop)
import qualified Graphics.UI.GLFW.Events as GLFWEvents

import           Prelude.Compat

data AnimConfig = AnimConfig
    { acTimePeriod :: NominalDiffTime
    , acRemainingRatioInPeriod :: Anim.R
    }

data EventResult =
    ERNone | ERRefresh | ERQuit
    deriving (Eq, Ord, Show)
instance Monoid EventResult where
    mempty = ERNone
    mappend = max

type Event = GLFWEvents.KeyEvent

data ImageHandlers = ImageHandlers
    { imageEventHandler :: Event -> IO ()
    , imageUpdate :: IO (Maybe Image)
    , imageRefresh :: IO Image
    }

windowSize :: GLFW.Window -> IO Widget.Size
windowSize win =
    do
        (x, y) <- GLFW.getFramebufferSize win
        return $ fromIntegral <$> Vector2 x y

mainLoopImage :: GLFW.Window -> (Widget.Size -> ImageHandlers) -> IO ()
mainLoopImage win imageHandlers =
    do
        initialSize <- windowSize win
        frameBufferSize <- newIORef initialSize
        drawnImageHandlers <- imageHandlers initialSize & newIORef
        let handleEvent (GLFWEvents.EventKey keyEvent) =
                do
                    handlers <- readIORef drawnImageHandlers
                    imageEventHandler handlers keyEvent
                    return ERNone
            handleEvent GLFWEvents.EventWindowClose = return ERQuit
            handleEvent GLFWEvents.EventWindowRefresh = return ERRefresh
            handleEvent (GLFWEvents.EventFrameBufferSize size) =
                do
                    writeIORef frameBufferSize (fromIntegral <$> size)
                    return ERRefresh
        let handleEvents events =
                do
                    eventResult <- mconcat <$> traverse handleEvent events
                    winSize <- readIORef frameBufferSize
                    let handlers = imageHandlers winSize
                    writeIORef drawnImageHandlers handlers
                    case eventResult of
                        ERQuit -> return ResultQuit
                        ERRefresh -> imageRefresh handlers >>= draw winSize
                        ERNone ->
                            imageUpdate handlers >>=
                            maybe (return ResultNone) (draw winSize)
        eventLoop win handleEvents
    where
        draw winSize@(Vector2 winSizeX winSizeY) image =
            do
                GL.viewport $=
                    (GL.Position 0 0,
                     GL.Size (round winSizeX) (round winSizeY))
                image
                    & (DrawUtils.translate (Vector2 (-1) 1) <>
                       DrawUtils.scale (Vector2 (2/winSizeX) (-2/winSizeY)) %%)
                    & let Vector2 glPixelRatioX glPixelRatioY = winSize / 2 -- GL range is -1..1
                      in DrawUtils.clearRenderSized (glPixelRatioX, glPixelRatioY)
                return ResultDidDraw

data AnimHandlers = AnimHandlers
    { animTickHandler :: IO (Maybe (Monoid.Endo AnimId))
    , animEventHandler :: Event -> IO (Maybe (Monoid.Endo AnimId))
    , animMakeFrame :: IO Anim.Frame
    }

data IsAnimating
    = Animating NominalDiffTime -- Current animation speed half-life
    | FinalFrame
    | NotAnimating
    deriving Eq

asyncThrowTo :: E.Exception e => ThreadId -> e -> IO ()
asyncThrowTo threadId exc = void $ forkIOUnmasked $ E.throwTo threadId exc

withForkedIO :: IO () -> IO a -> IO a
withForkedIO action =
    E.bracket (forkIOUnmasked action) (`asyncThrowTo` E.ThreadKilled) . const

data ThreadSyncVar = ThreadSyncVar
    { _tsvHaveTicks :: !Bool
    , _tsvRefreshRequested :: !Bool
    , _tsvWinSize :: !Widget.Size
    , _tsvReversedEvents :: [Event]
    }

tsvHaveTicks :: Lens' ThreadSyncVar Bool
tsvHaveTicks f ThreadSyncVar {..} = f _tsvHaveTicks <&> \_tsvHaveTicks -> ThreadSyncVar {..}

tsvRefreshRequested :: Lens' ThreadSyncVar Bool
tsvRefreshRequested f ThreadSyncVar {..} = f _tsvRefreshRequested <&> \_tsvRefreshRequested -> ThreadSyncVar {..}

tsvWinSize :: Lens' ThreadSyncVar Widget.Size
tsvWinSize f ThreadSyncVar {..} = f _tsvWinSize <&> \_tsvWinSize -> ThreadSyncVar {..}

tsvReversedEvents :: Lens' ThreadSyncVar [Event]
tsvReversedEvents f ThreadSyncVar {..} = f _tsvReversedEvents <&> \_tsvReversedEvents -> ThreadSyncVar {..}

-- Animation thread will have not only the cur frame, but the dest
-- frame in its mutable current state (to update it asynchronously)

-- Worker thread receives events, ticks (which may be lost), handles them, responds to animation thread
-- Animation thread sends events, ticks to worker thread. Samples results from worker thread, applies them to the cur state

data AnimState = AnimState
    { _asIsAnimating :: !IsAnimating
    , _asCurTime :: !UTCTime
    , _asCurFrame :: !Anim.Frame
    , _asDestFrame :: !Anim.Frame
    }

asIsAnimating :: Lens' AnimState IsAnimating
asIsAnimating f AnimState {..} = f _asIsAnimating <&> \_asIsAnimating -> AnimState {..}

asCurFrame :: Lens' AnimState Anim.Frame
asCurFrame f AnimState {..} = f _asCurFrame <&> \_asCurFrame -> AnimState {..}

asCurTime :: Lens' AnimState UTCTime
asCurTime f AnimState {..} = f _asCurTime <&> \_asCurTime -> AnimState {..}

asDestFrame :: Lens' AnimState Anim.Frame
asDestFrame f AnimState {..} = f _asDestFrame <&> \_asDestFrame -> AnimState {..}

initialAnimState :: Anim.Frame -> IO AnimState
initialAnimState initialFrame =
    do
        curTime <- getCurrentTime
        return AnimState
            { _asIsAnimating = NotAnimating
            , _asCurTime = curTime
            , _asCurFrame = initialFrame
            , _asDestFrame = initialFrame
            }

forwardExceptions :: IO a -> IO (IO a)
forwardExceptions action =
    do
        selfId <- myThreadId
        return $ action `E.catch` \exc@E.SomeException{} ->
            do
                asyncThrowTo selfId exc
                E.throwIO exc

desiredFrameRate :: Num a => a
desiredFrameRate = 60

mainLoopAnim :: GLFW.Window -> IO AnimConfig -> (Widget.Size -> AnimHandlers) -> IO ()
mainLoopAnim win getAnimationConfig animHandlers =
    do
        initialWinSize <- windowSize win
        frameStateVar <-
            animMakeFrame (animHandlers initialWinSize)
            >>= initialAnimState >>= newTVarIO
        eventTVar <-
            newTVarIO ThreadSyncVar
            { _tsvHaveTicks = False
            , _tsvRefreshRequested = False
            , _tsvWinSize = initialWinSize
            , _tsvReversedEvents = []
            }
        eventHandler <- forwardExceptions (eventHandlerThread frameStateVar eventTVar getAnimationConfig animHandlers)
        withForkedIO eventHandler $ mainLoopAnimThread frameStateVar eventTVar win

waitForEvent :: TVar ThreadSyncVar -> IO ThreadSyncVar
waitForEvent eventTVar =
    do
        tsv <- readTVar eventTVar
        when (not (tsv ^. tsvHaveTicks) &&
              not (tsv ^. tsvRefreshRequested) &&
              null (tsv ^. tsvReversedEvents))
            STM.retry
        tsv
            & tsvHaveTicks .~ False
            & tsvRefreshRequested .~ False
            & tsvReversedEvents .~ []
            & writeTVar eventTVar
        return tsv
    & STM.atomically

eventHandlerThread :: TVar AnimState -> TVar ThreadSyncVar -> IO AnimConfig -> (Widget.Size -> AnimHandlers) -> IO ()
eventHandlerThread frameStateVar eventTVar getAnimationConfig animHandlers =
    forever $
    do
        tsv <- waitForEvent eventTVar
        userEventTime <- getCurrentTime
        let handlers = animHandlers (tsv ^. tsvWinSize)
        eventResults <-
            mapM (animEventHandler handlers) $ reverse (tsv ^. tsvReversedEvents)
        tickResult <-
            if tsv ^. tsvHaveTicks
            then animTickHandler handlers
            else return Nothing
        case (tsv ^. tsvRefreshRequested, mconcat (tickResult : eventResults)) of
            (False, Nothing) -> return ()
            (_, mMapping) ->
                do
                    destFrame <- animMakeFrame handlers
                    AnimConfig timePeriod ratio <- getAnimationConfig
                    curTime <- getCurrentTime
                    let timeRemaining =
                            max 0 $
                            diffUTCTime
                            (addUTCTime timePeriod userEventTime)
                            curTime
                    let animationHalfLife = timeRemaining / realToFrac (logBase 0.5 ratio)
                    STM.atomically $ modifyTVar frameStateVar $
                        \oldFrameState ->
                        oldFrameState
                        & asIsAnimating .~ Animating animationHalfLife
                        & asDestFrame .~ destFrame
                        -- retroactively pretend animation started at
                        -- user event time to make the
                        -- animation-until-dest-frame last the same
                        -- amount of time no matter how long it took
                        -- to handle the event:
                        & asCurTime .~ addUTCTime (-1.0 / desiredFrameRate) curTime
                        & asCurFrame %~ Anim.mapIdentities (Monoid.appEndo (fromMaybe mempty mMapping))
                    -- In case main thread went to sleep (not knowing
                    -- whether to anticipate a tick result), wake it
                    -- up
                    GLFW.postEmptyEvent

mainLoopAnimThread :: TVar AnimState -> TVar ThreadSyncVar -> GLFW.Window -> IO ()
mainLoopAnimThread frameStateVar eventTVar win =
    mainLoopImage win $ \size ->
    ImageHandlers
    { imageEventHandler = \event -> (tsvReversedEvents %~ (event :)) & updateTVar
    , imageRefresh =
        do
            updateTVar (tsvRefreshRequested .~ True)
            updateFrameState size <&> _asCurFrame <&> Anim.draw
    , imageUpdate = updateFrameState size <&> frameStateResult
    }
    where
        updateTVar = STM.atomically . modifyTVar eventTVar
        tick size = updateTVar $ (tsvHaveTicks .~ True) . (tsvWinSize .~ size)
        updateFrameState size =
            do
                tick size
                curTime <- getCurrentTime
                STM.atomically $
                    do
                        AnimState prevAnimating prevTime prevFrame destFrame <-
                            readTVar frameStateVar
                        let notAnimating = AnimState NotAnimating curTime destFrame destFrame
                            newAnimState =
                                case prevAnimating of
                                Animating animationHalfLife ->
                                    case Anim.nextFrame progress destFrame prevFrame of
                                    Nothing -> AnimState FinalFrame curTime destFrame destFrame
                                    Just newFrame -> AnimState (Animating animationHalfLife) curTime newFrame destFrame
                                    where
                                        elapsed = curTime `diffUTCTime` prevTime
                                        progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac animationHalfLife)
                                FinalFrame -> notAnimating
                                NotAnimating -> notAnimating
                        writeTVar frameStateVar newAnimState
                        return newAnimState
        frameStateResult (AnimState isAnimating _ frame _) =
            case isAnimating of
            Animating _ -> Just $ Anim.draw frame
            FinalFrame -> Just $ Anim.draw frame
            NotAnimating -> Nothing

mainLoopWidget :: GLFW.Window -> IO Bool -> (Widget.Size -> IO (Widget IO)) -> IO AnimConfig -> IO ()
mainLoopWidget win widgetTickHandler mkWidgetUnmemod getAnimationConfig =
    do
        mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
        let newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
            getWidget size = ($ size) =<< readIORef mkWidgetRef
        mainLoopAnim win getAnimationConfig $ \size -> AnimHandlers
            { animTickHandler =
                do
                    anyUpdate <- widgetTickHandler
                    when anyUpdate newWidget
                    widget <- getWidget size
                    tickResults <-
                        sequenceA (widget ^. Widget.eventMap . E.emTickHandlers)
                    unless (null tickResults) newWidget
                    return $
                        case (tickResults, anyUpdate) of
                        ([], False) -> Nothing
                        _ -> Just . mconcat $ map (^. Widget.eAnimIdMapping) tickResults
            , animEventHandler = \event ->
                do
                    widget <- getWidget size
                    let eventMap = widget ^. Widget.eventMap
                    mAnimIdMapping <-
                        case E.lookupPasteHandler event eventMap of
                        Nothing -> E.lookup event eventMap & sequenceA
                        Just pasteHandler ->
                            GLFW.getClipboardString win
                            >>= Lens._Just pasteHandler
                        <&> fmap (^. Widget.eAnimIdMapping)
                    case mAnimIdMapping of
                        Nothing -> return ()
                        Just _ -> newWidget
                    return mAnimIdMapping
            , animMakeFrame = getWidget size <&> (^. Widget.animFrame)
            }
