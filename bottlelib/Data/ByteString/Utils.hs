module Data.ByteString.Utils
    ( lazifyBS, strictifyBS, randomBS, xorBS, ljust
    ) where

import           Control.Monad (replicateM)
import           Data.Bits (xor)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.Function (on)
import           Data.Word (Word8)
import           System.Random (randomIO)

strictifyBS :: LBS.ByteString -> SBS.ByteString
strictifyBS = SBS.concat . LBS.toChunks

lazifyBS :: SBS.ByteString -> LBS.ByteString
lazifyBS = LBS.fromChunks . return

randomBS :: Int -> IO SBS.ByteString
randomBS l = SBS.pack `fmap` replicateM l randomIO

ljust :: Int -> Word8 -> SBS.ByteString -> SBS.ByteString
ljust len chr s = s `SBS.append` SBS.replicate (len - SBS.length s) chr

xorBS :: SBS.ByteString -> SBS.ByteString -> SBS.ByteString
xorBS x y =
    SBS.pack $ on (SBS.zipWith xor) (ljust l 0) x y
    where
        l = on max SBS.length x y
