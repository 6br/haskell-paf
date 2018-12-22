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
import qualified Bio.Sam.Cigar as CIG
import qualified Bio.Sam.RawSam as R
import Bio.Sam.Parse
import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((|>))
import Control.Monad
import qualified Data.Vector as V
import Data.Attoparsec.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base16
import Data.Either
import Data.Int
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector.Unboxed as UV
import GHC.Float

alnParser :: Parser P.Aln
alnParser = do
  alnWithoutOpt <- P.Aln <$>
                   qnameP  <* tabP <*>
                   qlenP   <* tabP <*>
                   qstartP <* tabP <*>
                   qendP   <* tabP <*>
                   strandP <* tabP <*>
                   tnameP  <* tabP <*>
                   tlenP <* tabP <*>
                   tstartP <* tabP <*>
                   tendP   <* tabP <*>
                   rmatchP <* tabP <*>
                   ablockP <* tabP <*>
                   mapqP
  (opt, mLongCigars) <- optP
  return $ fillLongCigars (alnWithoutOpt opt Nothing) mLongCigars

optP :: Parser (V.Vector R.AlnOpt, Maybe (UV.Vector CIG.Cigar))
optP = first V.fromList . findOptCigars <$> many (tabP *> foldl1 (<|>) [optCharP,
                                                                        optIntP,
                                                                        optFloatP,
                                                                        optStringP,
                                                                        optByteArrayP,
                                                                        optArrayP])

findOptCigars :: [R.AlnOpt] -> ([R.AlnOpt], Maybe (UV.Vector CIG.Cigar))
findOptCigars [] = ([], Nothing)
findOptCigars (R.AlnOpt "cg" (R.AlnOptString cigars):xs) = (xs, join $ maybeResult $ feed (parse cigarsP cigars) "" )
findOptCigars (R.AlnOpt "cg" _:_) = error "\"CG\" tag should have String as its value"
findOptCigars (x:xs) = (x:xs', cs) where (xs', cs) = findOptCigars xs

qnameP :: Parser T.Text
qnameP = decodeUtf8 <$> takeWhile ('!' <-> '?' <||> 'A' <-> '~')

qlenP :: Parser Int32
qlenP = signed decimal

qstartP :: Parser Int32
qstartP = signed decimal

qendP :: Parser Int32
qendP = signed decimal

tnameP :: Parser T.Text
tnameP = decodeUtf8 <$> takeWhile ('!' <-> '?' <||> 'A' <-> '~')

tlenP :: Parser Int32
tlenP = signed decimal

tstartP :: Parser Int32
tstartP = signed decimal

tendP :: Parser Int32
tendP = signed decimal

rmatchP :: Parser Int32
rmatchP = signed decimal

ablockP :: Parser Int32
ablockP = signed decimal

mapqP :: Parser Int8
mapqP = signed decimal

strandP :: Parser Char
strandP = char '+' <|> char '-'

tabP :: Parser Char
tabP = char '\t'

cigar1P :: Parser CIG.Cigar
cigar1P = CIG.Cigar <$> decimal <*> (CIG.fromChar <$> satisfy (inClass' "MIDNSHP=X"))

-- should make sure there exists at least one CIGAR if the filed is not '*'
cigarsP :: Parser (Maybe (UV.Vector CIG.Cigar))
cigarsP = starOr $ Just . UV.fromList <$> many cigar1P

starOr :: Parser (Maybe a) -> Parser (Maybe a)
starOr p = Nothing <$ "*"
           <|> p

isHex :: Char -> Bool
isHex = isDigit <||> 'A' <-> 'F' <||> 'a' <-> 'f'

opt1P :: Char -> Parser R.AlnOptValue -> Parser R.AlnOpt
opt1P c p = R.AlnOpt <$> tagP <* char ':' <* char c <* char ':' <*> p
  where tagP = T.pack <$> satisfy isAlpha_ascii <:> satisfy (isAlpha_ascii <||> isDigit) <:> pure []

optCharP :: Parser R.AlnOpt
optCharP = opt1P 'A' $ R.AlnOptChar <$> anyChar

optIntP :: Parser R.AlnOpt
optIntP = opt1P 'i' $ R.AlnOptInt32 <$> signed decimal

optFloatP :: Parser R.AlnOpt
optFloatP = opt1P 'f' $ R.AlnOptFloat . double2Float <$> signed double

optStringP :: Parser R.AlnOpt
optStringP = opt1P 'Z' $ R.AlnOptString <$> takeWhile (' ' <-> '~')

optByteArrayP :: Parser R.AlnOpt
optByteArrayP = opt1P 'H' $ do
  (result, invalid) <- decode <$> takeWhile isHex
  guard $ B8.null invalid
  return $ R.AlnOptByteArray result

optArrayP :: Parser R.AlnOpt
optArrayP = opt1P 'B' $
  foldl1 (<|>) [valueP 'c' R.AlnOptInt8Array  $ signed decimal,
                valueP 'C' R.AlnOptUInt8Array   decimal,
                valueP 's' R.AlnOptInt16Array $ signed decimal,
                valueP 'S' R.AlnOptUInt16Array  decimal,
                valueP 'i' R.AlnOptInt32Array $ signed decimal,
                valueP 'I' R.AlnOptUInt32Array  decimal,
                valueP 'f' R.AlnOptFloatArray $ double2Float <$> signed double
               ]
  where
    valueP :: UV.Unbox a => Char -> (UV.Vector a -> R.AlnOptValue) -> Parser a -> Parser R.AlnOptValue
    valueP c t p = char c *> (t . UV.fromList <$> many (char ',' *> p))

fillLongCigars :: P.Aln -> Maybe (UV.Vector CIG.Cigar) -> P.Aln
fillLongCigars aln Nothing           = aln
fillLongCigars aln (Just longCigars) = aln & P.cigars .~ Just longCigars

pafParser :: Parser P.Paf
pafParser = P.Paf <$>
               (V.fromList <$> alnParser `sepBy` many' endOfLine) <* 
               option () (endOfLine <* skipSpace) -- the last EOL is not necessarily required
               <* endOfInput
