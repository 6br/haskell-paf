module Bio.Paf.IO
  (
   readPafFile,
   readPafStdin,
--   writePafFile,
--   writePafStdout
  )
where

import Conduit
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString.Char8
import qualified Bio.Paf as P
--import Bio.Paf.Export
import Bio.Paf.Parse

readFileByParser :: Parser a -> FilePath -> IO a
readFileByParser parser path = runConduitRes $ sourceFileBS path .| sinkParser parser

readPafFile :: FilePath -> IO P.Paf
readPafFile = readFileByParser pafParser

readStdinByParser :: Parser a -> IO a
readStdinByParser parser = runConduitRes $ stdinC .| sinkParser parser

readPafStdin :: IO S.Paf
readPafStdin = readStdinByParser pafParser

--writePafFile :: FilePath -> S.Paf  -> IO ()
--writePafFile path paf = runConduitRes $ yield (exportPaf paf) .| encodeUtf8C .| sinkFileBS path

--writePafStdout :: S.Paf -> IO ()
--writePafStdout paf = runConduitRes $ yield (exportPaf paf) .| encodeUtf8C .| stdoutC
