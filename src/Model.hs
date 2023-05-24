{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Model (
    initialize,
    learn,
    predict,
) where

import Control.Monad as M
import Data.Convertible as Conv
import Data.List as List
import Data.Vector.Storable as Vec
import Numeric.LinearAlgebra

import Types
import Util

#define GRADIENT_DECENT_ITERATIONS 32
#define HIDDEN_LAYER_SIZE 32
#define INPUT_LAYER_SIZE 784
#define LEARNING_RATE 0.01
#define OUTPUT_LAYER_SIZE 10
#define STANDARD_DEVIATION 0.01

initialize :: IO Model
initialize = do
    w1 <- fmap (HIDDEN_LAYER_SIZE >< INPUT_LAYER_SIZE)
        $ M.replicateM (HIDDEN_LAYER_SIZE * INPUT_LAYER_SIZE)
        $ normal 0 STANDARD_DEVIATION
    w2 <- fmap (OUTPUT_LAYER_SIZE >< HIDDEN_LAYER_SIZE)
        $ M.replicateM (OUTPUT_LAYER_SIZE * HIDDEN_LAYER_SIZE)
        $ normal 0 STANDARD_DEVIATION
    return $ Model b1 b2 w1 w2
    where
    b1 = fromList $ List.replicate HIDDEN_LAYER_SIZE 0
    b2 = fromList $ List.replicate OUTPUT_LAYER_SIZE 0

forward :: Model -> Vector Float -> ForwardResult
forward Model {..} a0 =
    ForwardResult a1 a2 z1 z2
    where
    a1 = relu z1
    a2 = softmax z2
    z1 = w1 #> a0 + b1
    z2 = w2 #> a1 + b2

backward :: Model -> Vector Float -> Vector Float -> ForwardResult -> BackwardResult
backward Model {..} a0 y ForwardResult {..} =
    BackwardResult db1 db2 dw1 dw2
    where
    db1 = db2 <# w2 * reluDeriv z1
    db2 = a2 - y
    dw1 = outer db1 a0
    dw2 = outer db2 a1

update :: Int -> Model -> BackwardResult -> Model
update n Model {..} BackwardResult {..} =
    Model b1' b2' w1' w2'
    where
    b1' = b1 - scale α db1
    b2' = b2 - scale α db2
    w1' = w1 - scale α dw1
    w2' = w2 - scale α dw2
    α   = LEARNING_RATE / realToFrac n

learn :: Int -> Model -> (Vector Float, Vector Float) -> Model
learn n model (x, y) =
    List.foldl go model [1.. GRADIENT_DECENT_ITERATIONS :: Int]
    where go m _ = update n m $ backward m x y $ forward m x

predict :: Model -> Image -> Label
predict model = fromIntegral . Vec.maxIndex . a2 . forward model . Conv.convert
