{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.Sv.Lazy.Internal
  ( makeIbs
  , makeLazyCursor
  , makeCummulativePopCount
  , zip2Or
  , makeQuoteMask
  , zip2And
  , nextField
  , nextRow
  , snippet
  , nextPosition
  , countNexts
  , toListVector
  , toVectorVector
  ) where

import Data.Char                                 (ord)
import Data.Function
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Sv.Bits
import HaskellWorks.Data.Sv.Broadword
import HaskellWorks.Data.Sv.Lazy.Cursor
import Prelude

import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Vector                          as DV
import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.Sv.ByteString.Lazy as LBS
import qualified HaskellWorks.Data.Sv.Char.Word64     as CW
import qualified HaskellWorks.Data.Sv.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

makeIbs :: Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
makeIbs iw v = DVS.constructN ((DVS.length v + 7) `div` 8) go
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = DVS.length u in
          if ui * 8 + 8 < DVS.length v
            then  let vi  = ui * 8
                      w0  = testWord8s (DVS.unsafeIndex v (vi + 0) .^. iw)
                      w1  = testWord8s (DVS.unsafeIndex v (vi + 1) .^. iw)
                      w2  = testWord8s (DVS.unsafeIndex v (vi + 2) .^. iw)
                      w3  = testWord8s (DVS.unsafeIndex v (vi + 3) .^. iw)
                      w4  = testWord8s (DVS.unsafeIndex v (vi + 4) .^. iw)
                      w5  = testWord8s (DVS.unsafeIndex v (vi + 5) .^. iw)
                      w6  = testWord8s (DVS.unsafeIndex v (vi + 6) .^. iw)
                      w7  = testWord8s (DVS.unsafeIndex v (vi + 7) .^. iw)
                      w   = (w7 .<. 56) .|.
                            (w6 .<. 48) .|.
                            (w5 .<. 40) .|.
                            (w4 .<. 32) .|.
                            (w3 .<. 24) .|.
                            (w2 .<. 16) .|.
                            (w1 .<.  8) .|.
                             w0
                  in comp w
            else  let vi  = ui * 8
                      w0  = testWord8s (DVS.atIndexOr 0 v (vi + 0) .^. iw)
                      w1  = testWord8s (DVS.atIndexOr 0 v (vi + 1) .^. iw)
                      w2  = testWord8s (DVS.atIndexOr 0 v (vi + 2) .^. iw)
                      w3  = testWord8s (DVS.atIndexOr 0 v (vi + 3) .^. iw)
                      w4  = testWord8s (DVS.atIndexOr 0 v (vi + 4) .^. iw)
                      w5  = testWord8s (DVS.atIndexOr 0 v (vi + 5) .^. iw)
                      w6  = testWord8s (DVS.atIndexOr 0 v (vi + 6) .^. iw)
                      w7  = testWord8s (DVS.atIndexOr 0 v (vi + 7) .^. iw)
                      w   = (w7 .<. 56) .|.
                            (w6 .<. 48) .|.
                            (w5 .<. 40) .|.
                            (w4 .<. 32) .|.
                            (w3 .<. 24) .|.
                            (w2 .<. 16) .|.
                            (w1 .<.  8) .|.
                            w0
                  in comp w
{-# INLINE makeIbs #-}

zipOr :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
zipOr as bs = DVS.constructN (DVS.length as `max` DVS.length bs) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = DVS.length u
          in if ui < DVS.length as && ui < DVS.length bs
            then DVS.unsafeIndex as ui .|. DVS.unsafeIndex bs ui
            else error "Different sized vectors"
{-# INLINE zipOr #-}

zip2Or :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
zip2Or (a:as) (b:bs) = zipOr a b:zip2Or as bs
zip2Or (a:as) []     = a:zip2Or as []
zip2Or []     (b:bs) = b:zip2Or [] bs
zip2Or []     []     = []
{-# INLINE zip2Or #-}

zipAnd :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
zipAnd as bs = DVS.constructN (DVS.length as `max` DVS.length bs) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = DVS.length u
          in if ui < DVS.length as && ui < DVS.length bs
            then DVS.unsafeIndex as ui .&. DVS.unsafeIndex bs ui
            else error "Different sized vectors"
{-# INLINE zipAnd #-}

zip2And :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
zip2And (a:as) (b:bs) = zipAnd a b:zip2And as bs
zip2And (a:as) []     = a:zip2And as []
zip2And []     (b:bs) = b:zip2And [] bs
zip2And []     []     = []
{-# INLINE zip2And #-}

makeQuoteMask1 :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
makeQuoteMask1 ibv pcv = DVS.constructN (DVS.length ibv) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = DVS.length u in
          toggle64 (DVS.unsafeIndex pcv ui) (DVS.unsafeIndex ibv ui)
{-# INLINE makeQuoteMask1 #-}

makeQuoteMask :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
makeQuoteMask (ibv:ibvs) (pcv:pcvs) = makeQuoteMask1 ibv pcv:makeQuoteMask ibvs pcvs
makeQuoteMask _ _                   = []
{-# INLINE makeQuoteMask #-}

makeCummulativePopCount2 :: Word64 -> DVS.Vector Word64 -> (DVS.Vector Word64, Word64)
makeCummulativePopCount2 c v = let r = DVS.constructN (DVS.length v + 1) go in (DVS.unsafeInit r, DVS.unsafeLast r)
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = DVS.length u in
          if ui > 0
            then popCount1 (DVS.unsafeIndex v (ui - 1)) + DVS.unsafeIndex u (ui - 1)
            else c
{-# INLINE makeCummulativePopCount2 #-}

makeCummulativePopCount :: [DVS.Vector Word64] -> [DVS.Vector Word64]
makeCummulativePopCount = go 0
  where go :: Word64 -> [DVS.Vector Word64] -> [DVS.Vector Word64]
        go c (v:vs) = let (u, c') = makeCummulativePopCount2 c v in u:go c' vs
        go _ []     = []
{-# INLINE makeCummulativePopCount #-}

makeLazyCursor :: Char -> LBS.ByteString -> SvCursor9
makeLazyCursor delimiter lbs = SvCursor9
  { svCursor9Delimiter     = fromIntegral (ord delimiter)
  , svCursor9Text          = lbs
  , svCursor9InterestBits  = ib
  , svCursor9Newlines      = nls
  , svCursor9Position      = 1
  }
  where ws  = LBS.toVector64Chunks 512 lbs
        ibq = makeIbs CW.doubleQuote                      <$> ws
        ibn = makeIbs CW.newline                          <$> ws
        ibd = makeIbs (CW.fillWord64WithChar8 delimiter)  <$> ws
        pcq = makeCummulativePopCount ibq
        ibr = zip2Or ibn ibd
        qm  = makeQuoteMask ibq pcq
        ib  = zip2And ibr qm
        nls = zip2And ibn qm
{-# INLINE makeLazyCursor #-}

snippet :: SvCursor9 -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ svCursor9Text c
  where d = nextField c
        posC = fromIntegral $ svCursor9Position c - 1
        posD = fromIntegral $ svCursor9Position d - 1
        len  = posD - posC
{-# INLINE snippet #-}

countNexts :: SvCursor9 -> Int
countNexts = go 0
  where go n d = if not (atEnd d)
          then let e = nextField d in if not (atEnd e)
            then let f = nextPosition e in go (n + 1) (trimCursor f)
            else n
          else n
        {-# INLINE go #-}
{-# INLINE countNexts #-}

trimCursor :: SvCursor9 -> SvCursor9
trimCursor c = if svCursor9Position c > 512
  then trimCursor c
    { svCursor9Text         = LBS.drop 512 (svCursor9Text c)
    , svCursor9InterestBits = drop 1 (svCursor9InterestBits c)
    , svCursor9Newlines     = drop 1 (svCursor9Newlines c)
    , svCursor9Position     = svCursor9Position c - 512
    }
  else c
{-# INLINE trimCursor #-}

atEnd :: SvCursor9 -> Bool
atEnd c = LBS.null (LBS.drop (fromIntegral (svCursor9Position c)) (svCursor9Text c))
{-# INLINE atEnd #-}

nextField :: SvCursor9 -> SvCursor9
nextField cursor = cursor
  { svCursor9Position = newPos
  }
  where currentRank = rank1   (svCursor9InterestBits cursor) (svCursor9Position cursor)
        newPos      = select1 (svCursor9InterestBits cursor) (currentRank + 1)
{-# INLINE nextField #-}

nextRow :: SvCursor9 -> SvCursor9
nextRow cursor = cursor
  { svCursor9Position = newPos
  }
  where currentRank = rank1   (svCursor9Newlines cursor) (svCursor9Position cursor)
        newPos      = select1 (svCursor9Newlines cursor) (currentRank + 1)
{-# INLINE nextRow #-}

nextPosition :: SvCursor9 -> SvCursor9
nextPosition cursor = cursor
    { svCursor9Position = if LBS.null (LBS.drop (fromIntegral newPos) (svCursor9Text cursor))
                            then fromIntegral (LBS.length (svCursor9Text cursor))
                            else newPos
    }
  where newPos  = svCursor9Position cursor + 1
{-# INLINE nextPosition #-}

mkRow :: SvCursor9 -> SvCursor9 -> DV.Vector LBS.ByteString
mkRow c d = DV.unfoldrN c2d go c
  where cr  = rank1 (svCursor9InterestBits c) (svCursor9Position c)
        dr  = rank1 (svCursor9InterestBits d) (svCursor9Position d)
        c2d = fromIntegral (dr - cr)
        go :: SvCursor9 -> Maybe (LBS.ByteString, SvCursor9)
        go e = case nextField e of
          f -> case snippet e of
            s -> case nextPosition f of
              g -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE mkRow #-}

toListVector :: SvCursor9 -> [DV.Vector LBS.ByteString]
toListVector c = if svCursor9Position d > svCursor9Position c && not (atEnd c)
  then mkRow c d:toListVector (trimCursor d)
  else []
  where d = nextRow c
{-# INLINE toListVector #-}

toVectorVector :: SvCursor9 -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
