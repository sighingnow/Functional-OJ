import           Control.Monad
import           Data.List
import           Data.HashTable.IO (CuckooHashTable)
import qualified Data.HashTable.IO as Hash

main :: IO ()
main = do
    mapping <- Hash.new :: IO (CuckooHashTable String Int)
    n <- read <$> getLine
    replicateM_ n $ do
        s <- getLine
        r <- Hash.lookup mapping s
        case r of
          Nothing -> Hash.insert mapping s 1
          Just k -> Hash.insert mapping s (k + 1)
    m <- read <$> getLine
    replicateM_ m $ do
        s <- getLine
        r <- Hash.lookup mapping s
        case r of
          Just k -> print k
          Nothing -> print 0

