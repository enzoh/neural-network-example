module Util (
    normal,
    relu,
    reluDeriv,
    softmax,
) where

import Data.Vector.Storable as Vec
import Numeric.LinearAlgebra
import System.Random

normal :: Float -> Float -> IO Float
normal μ σ = do
    u1 <- randomIO
    u2 <- randomIO
    return $ μ + σ * sqrt (-2 * log u1) * cos (2 * pi * u2)

relu :: Vector Float -> Vector Float
relu = Vec.map $ max 0

reluDeriv :: Vector Float -> Vector Float
reluDeriv = Vec.map $ realToFrac . fromEnum . (<) 0

softmax :: Vector Float -> Vector Float
softmax x = let e = Vec.map exp x; α = 1 / sumElements e in scale α e
