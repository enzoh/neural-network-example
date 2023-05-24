{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types (
    Image(..),
    Label(..),
    Model(..),
    ForwardResult(..),
    BackwardResult(..),
) where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.ByteString as Byte
import Data.ByteString.Char8 as Char
import Data.Convertible
import Data.List as List
import Data.Semigroup as Assoc
import Data.Vector.Storable as Vec
import Data.Word
import Numeric.LinearAlgebra

newtype Image = Image { unboxImage :: ByteString }

instance Convertible Image (Vector Float) where
    safeConvert = Right . fromImage

instance Show Image where
    show = showImage

newtype Label = Label { unboxLabel :: Word8 }
    deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Convertible Label (Vector Float) where
    safeConvert = Right . fromLabel

instance Show Label where
    show = show . unboxLabel

data Model = Model {
    b1 :: !(Vector Float),
    b2 :: !(Vector Float),
    w1 :: !(Matrix Float),
    w2 :: !(Matrix Float)
}

instance Serialise Model where

    encode Model {..} =
        encodeListLen 4
            Assoc.<> encodeVector b1
            Assoc.<> encodeVector b2
            Assoc.<> encodeMatrix w1
            Assoc.<> encodeMatrix w2

    decode = do
        _ <- decodeListLen
        Model
            <$> decodeVector
            <*> decodeVector
            <*> decodeMatrix
            <*> decodeMatrix

data ForwardResult = ForwardResult {
    a1 :: !(Vector Float),
    a2 :: !(Vector Float),
    z1 :: !(Vector Float),
    z2 :: !(Vector Float)
}

data BackwardResult = BackwardResult {
    db1 :: !(Vector Float),
    db2 :: !(Vector Float),
    dw1 :: !(Matrix Float),
    dw2 :: !(Matrix Float)
}

decodeMatrix :: Decoder s (Matrix Float)
decodeMatrix = fromLists <$> decode

decodeVector :: Decoder s (Vector Float)
decodeVector = fromList <$> decode

encodeMatrix :: Matrix Float -> Encoding
encodeMatrix = encode . toLists

encodeVector :: Vector Float -> Encoding
encodeVector = encode . Vec.toList

fromImage :: Image -> Vector Float
fromImage = fromList . List.map realToFrac . Byte.unpack . unboxImage

fromLabel :: Label -> Vector Float
fromLabel label = generate 10 $ realToFrac . fromEnum . (==) n
    where n = fromIntegral label

showImage :: Image -> String
showImage = go . Byte.map shade . unboxImage
    where
    go bytes
        | Byte.null bytes = []
        | otherwise =
            let (a, b) = Byte.splitAt 28 bytes in
            Char.unpack a List.++ '\n' : go b

shade :: Word8 -> Word8
shade x = Byte.index " .:oO@" $ 6 * fromIntegral x `div` 256
