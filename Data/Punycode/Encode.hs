{-# LANGUAGE FlexibleContexts #-}

module Data.Punycode.Encode (encode) where

import           Control.Monad.State hiding (state)
import           Control.Monad.Writer
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Word

import           Data.Punycode.Shared

data PunycodeState = PunycodeState { n :: Int
                                   , delta :: Int
                                   , bias :: Int
                                   , h :: Int
                                   }

-- | Encode a string into its ascii form
encode :: String -> BS.ByteString
encode input = execWriter $ initialWriter input

initialWriter :: MonadWriter BS.ByteString m => String -> m ()
initialWriter input = do
  tell basics
  when (b > 0) $ tell $ BS.singleton $ fromIntegral $ ord '-'
  evalStateT (inner3 (map ord input) b) $ PunycodeState { n = initial_n
                                                             , delta = 0
                                                             , bias = initial_bias
                                                             , h = b
                                                             }
  where basics = BS.pack $ map (fromIntegral . ord) $ filter isBasic input
        b = BS.length basics

inner3 :: (MonadState PunycodeState m, MonadWriter BS.ByteString m) => [Int] -> Int -> m ()
inner3 input b = do
  state <- get
  helper state
  where helper state
          | h' < length input = do
            put $ state {n = m, delta = delta'}
            mapM_ (inner2 b) input
            state' <- get
            put $ state' {delta = (delta state') + 1, n = (n state') + 1}
            inner3 input b
          | otherwise = return ()
          where m = minimum $ filter (>= n') input
                n' = n state
                h' = h state
                delta' = (delta state) + (m - n') * (h' + 1)

inner2 :: (MonadState PunycodeState m, MonadWriter BS.ByteString m) => Int -> Int -> m ()
inner2 b c = do
  state <- get
  helper state
  where helper state
          | c == n' = do
            q <- inner delta' base bias'
            tell $ BS.singleton $ baseToAscii q
            put $ state {bias = adapt delta' (h' + 1) (h' == b), delta = 0, h = (h state) + 1}
          | otherwise = put $ state {delta = delta'}
          where delta' = (delta state) + d
                  where d
                          | c < n' = 1
                          | otherwise = 0
                n' = n state
                bias' = bias state
                h' = h state

inner :: (MonadWriter BS.ByteString m) => Int -> Int -> Int -> m Int
inner q k bias'
  | q < t = return q
  | otherwise = do
    tell $ BS.singleton $ baseToAscii $ t + ((q - t) `mod` (base - t))
    inner ((q - t) `div` (base - t)) (k + base) bias'
  where t
          | k <= bias' + tmin = tmin
          | k >= bias' + tmax = tmax
          | otherwise = k - bias'

isBasic :: Char -> Bool
isBasic = (< initial_n) . ord

baseToAscii :: Int -> Word8
baseToAscii i
  | i < 26 = fromIntegral $ i + (ord 'a')
  | otherwise = fromIntegral $ (i - 26) + (ord '0')
