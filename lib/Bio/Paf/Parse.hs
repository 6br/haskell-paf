{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Bio.Paf.Parse
  (
   pafParser
  )
where

import Prelude hiding (take, takeWhile)
import qualified Bio.Paf as P
import Bio.Sam.Parse
import Data.Attoparsec.Applicative
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8
import Data.Int
import qualified Data.Text as T
import Data.Text.Encoding

alnParser :: Parser P.Aln
alnParser = do
  alnWithoutOpt <- P.Aln <$>
                   qnameP  <* tabP <*>
                   qlenP   <* tabP <*>
                   qstartP <* tabP <*>
                   qendP   <* tabP <*>
                   strandP <* tabP <*>
                   tnameP  <* tabP <*>
                   tstartP <* tabP <*>
                   tendP   <* tabP <*>
                   rmatchP <* tabP <*>
                   mapqP
  (opt, mLongCigars) <- optP
  return $ fillLongCigars (alnWithoutOpt opt) mLongCigars
