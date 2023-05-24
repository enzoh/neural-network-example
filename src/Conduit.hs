module Conduit (
    sourceImages,
    sourceLabels,
) where

import Control.Monad.Trans.Resource
import Data.Attoparsec.ByteString as Atto
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary as Bin
import Data.Conduit.Combinators as C
import Data.Conduit.Zlib

import Types

sourceData :: FilePath -> Int -> Parser a -> ConduitT () a (ResourceT IO) ()
sourceData file n parser =
    sourceFile file .| ungzip .| go .| C.map snd
    where go = Bin.drop n >> conduitParser parser

sourceImages :: FilePath -> ConduitT () Image (ResourceT IO) ()
sourceImages file = sourceData file 16 $ Image <$> Atto.take 784

sourceLabels :: FilePath -> ConduitT () Label (ResourceT IO) ()
sourceLabels file = sourceData file 8 $ Label <$> anyWord8
