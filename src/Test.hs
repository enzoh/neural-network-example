{-# LANGUAGE CPP #-}

module Main where

import Codec.Serialise
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bool
import Data.ByteString.Lazy as Byte
import Data.Conduit
import Data.Conduit.Combinators as C
import Data.List as List
import System.Environment
import Text.Printf

import Conduit
import Model
import Types

-- #define TRACE 1

#define TEST_CASE_COUNT 10000
#define TEST_IMAGE_FILE "data/t10k-images-idx3-ubyte.gz"
#define TEST_LABEL_FILE "data/t10k-labels-idx1-ubyte.gz"

main :: IO ()
main = do
    args <- getArgs
    let file = if List.null args
        then "model.cbor"
        else List.head args
    contents <- Byte.readFile file
    let model = deserialise contents
    successes <- testModel model
    let accuracy = successes / TEST_CASE_COUNT * 100
    printf "Accuracy: %f%%\n" accuracy

testModel :: Model -> IO Float
testModel model = do
    let pipe = C.mapM $ uncurry $ testPrediction model
    let sink = flip C.foldl 0 $ flip $ bool id succ
    runResourceT $ runConduit $ source .| pipe .| sink
    where
    images = sourceImages TEST_IMAGE_FILE
    labels = sourceLabels TEST_LABEL_FILE
    source = labels .| mergeSource images

testPrediction :: MonadIO m => Model -> Image -> Label -> m Bool
testPrediction model image actual = do
    let expect = predict model image
    let result = expect == actual
#ifdef TRACE
    unless result $ liftIO $ do
        printf "%s\n" $ show image
        printf "Expect: %s\n" $ show expect
        printf "Actual: %s\n" $ show actual
#endif
    return result
