{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.RangeJoin
  ( cmdRangeJoin
  ) where

import App.Char
import App.Commands.Options.Parse
import Control.Lens
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.List
import Data.Maybe                   (catMaybes)
import Data.Semigroup               ((<>))
import Data.Word
import Options.Applicative          hiding (columns)
import Text.Read                    (readMaybe)

import qualified App.Commands.Options.Type              as Z
import qualified App.Data.RangeJoinColumn               as Z
import qualified App.IO                                 as IO
import qualified Data.ByteString.Builder                as B
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Data.Vector                            as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy as SVLL

rangeJoin :: (DV.Vector LBS.ByteString -> DV.Vector LBS.ByteString -> [LBS.ByteString])
  -> Int -> Int -> [DV.Vector LBS.ByteString] -> Int -> Int -> [DV.Vector LBS.ByteString] -> [[LBS.ByteString]]
rangeJoin f aa az as ba bz bs = rangeJoin' f (catMaybes (fmap (mkEntry aa az) as)) (catMaybes (fmap (mkEntry ba bz) bs))

mkEntry :: Int -> Int -> DV.Vector LBS.ByteString -> Maybe (Word32, Word32, DV.Vector LBS.ByteString)
mkEntry a z v = (,, v) <$> lookupWord32 a v <*> lookupWord32 z v

lookupWord32 :: Int -> DV.Vector LBS.ByteString -> Maybe Word32
lookupWord32 i v = do
  lbs <- v DV.!? i
  let s = T.unpack (T.decodeUtf8 (LBS.toStrict lbs))
  readMaybe s

rangeJoin' :: (DV.Vector LBS.ByteString -> DV.Vector LBS.ByteString -> [LBS.ByteString])
  -> [(Word32, Word32, DV.Vector LBS.ByteString)] -> [(Word32, Word32, DV.Vector LBS.ByteString)] -> [[LBS.ByteString]]
rangeJoin' f ((ua, uz, u):us) ((va, vz, v):vs) = if
  | uz < va   -> ("L":encodeWord32 ua:encodeWord32  uz     :f u e):rangeJoin' f                  us  ((va    , vz, v):vs)
  | vz < ua   -> ("R":encodeWord32 va:encodeWord32  vz     :f e v):rangeJoin' f ((ua    , uz, u):us) (                vs)
  | ua < va   -> ("L":encodeWord32 ua:encodeWord32 (va - 1):f u e):rangeJoin' f ((va    , uz, u):us) ((va    , vz, v):vs)
  | va < ua   -> ("R":encodeWord32 va:encodeWord32 (ua - 1):f e v):rangeJoin' f ((ua    , uz, u):us) ((ua    , vz, v):vs)
  | uz < vz   -> ("B":encodeWord32 ua:encodeWord32  uz     :f u v):rangeJoin' f                  us  ((uz + 1, vz, v):vs)
  | vz < uz   -> ("B":encodeWord32 va:encodeWord32  vz     :f u v):rangeJoin' f ((vz + 1, uz, u):us)                  vs
  | otherwise -> ("B":encodeWord32 va:encodeWord32  vz     :f u v):rangeJoin' f                  us                   vs
  where e = DV.empty
rangeJoin' f ((ua, uz, _):us) []                = ("L":encodeWord32 ua:encodeWord32 uz:[]):rangeJoin' f us []
rangeJoin' f []               ((va, vz, _):vs)  = ("R":encodeWord32 va:encodeWord32 vz:[]):rangeJoin' f [] vs
rangeJoin' _ []               []                = []

encodeWord32 :: Word32 -> LBS.ByteString
encodeWord32 = LBS.fromStrict . T.encodeUtf8 . T.pack . show

mkColumnSelector :: [Z.RangeJoinColumn] -> DV.Vector LBS.ByteString -> DV.Vector LBS.ByteString -> [LBS.ByteString]
mkColumnSelector []     _ _ = []
mkColumnSelector (c:cs) u v = case c of
  Z.LtColumn n -> (maybe "" id (u DV.!? n)):mkColumnSelector cs u v
  Z.RtColumn n -> (maybe "" id (v DV.!? n)):mkColumnSelector cs u v

runRangeJoin :: Z.RangeJoinOptions -> IO ()
runRangeJoin opts = do
  let input1Delimiter   = opts ^. the @"input1Delimiter"
  let input1FilePath    = opts ^. the @"input1FilePath"
  let input1StartColumn = opts ^. the @"input1StartColumn"
  let input1StopColumn  = opts ^. the @"input1StopColumn"
  let input2Delimiter   = opts ^. the @"input2Delimiter"
  let input2FilePath    = opts ^. the @"input2FilePath"
  let input2StartColumn = opts ^. the @"input2StartColumn"
  let input2StopColumn  = opts ^. the @"input2StopColumn"
  let columns           = opts ^. the @"columns"
  let outputFilePath    = opts ^. the @"outputFilePath"
  let outputDelimiter   = opts ^. the @"outputDelimiter"

  !rows1 <- SVLL.toListVector . SVL.makeCursor input1Delimiter <$> IO.readInputFile input1FilePath
  !rows2 <- SVLL.toListVector . SVL.makeCursor input2Delimiter <$> IO.readInputFile input2FilePath

  let !outDelimiterBuilder  = B.word8 outputDelimiter
  let !outNewlineBuilder    = B.word8 10
  let !columnSelector       = mkColumnSelector columns

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile outputFilePath Nothing
    let outLbsss  = rangeJoin columnSelector input1StartColumn input1StopColumn rows1 input2StartColumn input2StopColumn rows2
    let outBss    = fmap (fmap B.lazyByteString) outLbsss
    let outB      = mconcat (intersperse outNewlineBuilder (fmap (mconcat . intersperse outDelimiterBuilder) outBss))
    liftIO $ B.hPutBuilder hOut outB
  return ()

cmdRangeJoin :: Mod CommandFields (IO ())
cmdRangeJoin = command "range-join" $ flip info idm $ runRangeJoin <$> optsRangeJoin

optsRangeJoin :: Parser Z.RangeJoinOptions
optsRangeJoin = Z.RangeJoinOptions
    <$> strOption
        (   long "input1"
        <>  help "Input DSV file 1"
        <>  metavar "FILE"
        )
    <*> option readWord8
        (   long "input1-delimiter"
        <>  help "DSV delimiter to use for input1"
        <>  metavar "CHAR"
        )
    <*> nonZeroOneBased
        (   long "input1-start-column"
        <>  short 'k'
        <>  help "Column to use as start column from 1"
        <>  metavar "COLUMN INDEX"
        )
    <*> nonZeroOneBased
        (   long "input1-stop-column"
        <>  short 'k'
        <>  help "Column to use as stop column from 1"
        <>  metavar "COLUMN INDEX"
        )
    <*> strOption
          (   long "input2"
          <>  help "Input DSV file 2"
          <>  metavar "FILE"
          )
    <*> option readWord8
        (   long "input2-delimiter"
        <>  help "DSV delimiter to use for input2"
        <>  metavar "CHAR"
        )
    <*> nonZeroOneBased
        (   long "input2-start-column"
        <>  short 'k'
        <>  help "Column to use as start column from 2"
        <>  metavar "COLUMN INDEX"
        )
    <*> nonZeroOneBased
        (   long "input2-stop-column"
        <>  short 'k'
        <>  help "Column to use as stop column from 2"
        <>  metavar "COLUMN INDEX"
        )
    <*> many
        ( rangeJoinColumn
          (   long "column"
          <>  short 'k'
          <>  help "Range join colum"
          <>  metavar "RANGE_JOIN_COLUMN"
          )
        )
    <*> strOption
        (   long "range-type"
        <>  short 'r'
        <>  help "Range type"
        <>  metavar "RANGE_TYPE"
        )
    <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Output DSV file"
        <>  metavar "FILE"
        )
    <*> option readWord8
        (   long "output-delimiter"
        <>  short 'e'
        <>  help "DSV delimiter to write in the output"
        <>  metavar "CHAR"
        )
