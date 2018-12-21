import Bio.Paf
import Bio.Paf.IO

main :: IO ()
main = do
  sam <- readPafFile "test/data/test.paf"
  print sam
