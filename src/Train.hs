{-# LANGUAGE CPP #-}

module Main where

import Codec.Serialise
import Control.Arrow
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy as Byte
import Data.Conduit
import Data.Conduit.Combinators as C
import Data.Convertible

import Conduit
import Model
import Types

#define TRAIN_CASE_COUNT 60000
#define TRAIN_IMAGE_FILE "data/train-images-idx3-ubyte.gz"
#define TRAIN_LABEL_FILE "data/train-labels-idx1-ubyte.gz"

main :: IO ()
main = do
    model <- trainModel
    Byte.writeFile "model.cbor" $ serialise model

trainModel :: IO Model
trainModel = do
    model <- initialize
    let pipe = C.map $ convert *** convert
    let sink = C.foldl $ learn TRAIN_CASE_COUNT
    runResourceT $ runConduit $ source .| pipe .| sink model
    where
    images = sourceImages TRAIN_IMAGE_FILE
    labels = sourceLabels TRAIN_LABEL_FILE
    source = labels .| mergeSource images
