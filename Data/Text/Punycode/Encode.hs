{-# LANGUAGE FlexibleContexts #-}

module Data.Text.Punycode.Encode (encode) where

import           Control.Monad.State hiding (state)
import           Control.Monad.Writer
import qualified Data.ByteString as BS
import           Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word


data PunycodeState = PunycodeState 
  { n     :: {-# UNPACK #-} !Int
  , delta :: {-# UNPACK #-} !Int
  , bias  :: {-# UNPACK #-} !Int
  , h     :: {-# UNPACK #-} !Int
  }

-- | Encode a string into its ascii form
{-# INLINE encode #-}
encode :: T.Text -> BS.ByteString
encode = execWriter . initialWriter

{-# INLINE initialWriter #-}
initialWriter :: MonadWriter BS.ByteString m => T.Text -> m ()
initialWriter !input = do
  !_ <- tell basics
  !_ <- when (b > 0) . tell . BS.singleton . fromIntegral $! ord '-'
  !_ <- void . evalStateT (inner3 (map ord . T.unpack $! input) b) $!
    PunycodeState { n     = initial_n
                  , delta = 0
                  , bias  = initial_bias
                  , h     = b
                  }
  return ()
  where 
    basics :: BS.ByteString
    basics = TE.encodeUtf8 . T.filter isBasic $! input
    b :: Int
    b = BS.length basics

{-# INLINE inner3 #-}
inner3 :: (MonadState PunycodeState m, MonadWriter BS.ByteString m) => [Int] -> Int -> m ()
inner3 !input !b = do
  !state <- get
  void $! helper state
  where 
    helper !state
      | h' < length input = do
          !_ <- put $! state {n = m, delta = delta'}
          !_ <- void . flip mapM_ input $! inner2 b
          !state' <- get
          !_ <- void . put $! state' {delta = (delta state') + 1, n = (n state') + 1}
          !_ <- void $! inner3 input b
          return ()
      | otherwise = return ()
      where 
        m :: Int
        m = minimum . filter (>= n') $! input

        n' :: Int
        n' = n state

        h' :: Int
        h' = h state

        delta' :: Int
        delta' = (delta state) + (m - n') * (h' + 1)

{-# INLINE inner2 #-}
inner2 :: (MonadState PunycodeState m, MonadWriter BS.ByteString m) => Int -> Int -> m ()
inner2 !b !c = do
  !state <- get
  void $! helper state
  where 
    helper !state
      | c == n' = 
        do
          !q <- inner delta' base bias'
          !_ <- void . tell . BS.singleton . baseToAscii $! q
          !_ <- void . put $! state {bias = adapt delta' (h' + 1) (h' == b), delta = 0, h = (h state) + 1}
          return ()
      | otherwise = do
        !_ <- void . put $! state {delta = delta'}
        return ()
      where 
        delta' 
          | c < n' = 1 + delta state
          | otherwise = delta state

        n' :: Int
        n' = n state
        bias' :: Int
        bias' = bias state
        h' :: Int
        h' = h state

{-# INLINE inner #-}
inner :: (MonadWriter BS.ByteString m) => Int -> Int -> Int -> m Int
inner !q !k !bias'
  | q < t = return q
  | otherwise = do
    void . tell . BS.singleton . baseToAscii $! t + ((q - t) `mod` (base - t))
    !out <- inner ((q - t) `div` (base - t)) (k + base) bias'
    return out
  where 
    t :: Int
    t
      | k <= bias' + tmin = tmin
      | k >= bias' + tmax = tmax
      | otherwise = k - bias'

{-# INLINE baseToAscii #-}
baseToAscii :: Int -> Word8
baseToAscii !i
  | i < 26    = fromIntegral $! i + (ord 'a')
  | otherwise = fromIntegral $! (i - 26) + (ord '0')
