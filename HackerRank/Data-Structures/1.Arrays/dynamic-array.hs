import           Control.Monad
import           Data.List
import           Data.IORef
import           Data.Bits

import           Data.Vector.Unboxed.Mutable (IOVector(..), Unbox(..))
import qualified Data.Vector.Unboxed.Mutable as V

data DynArray a = DynArray {-# UNPACK #-}!(IORef Int)
                           {-# UNPACK #-}!(IORef Int)
                           (IORef (IOVector a))

createDyn :: Unbox a => Int -> IO (DynArray a)
createDyn cap = do
    used <- newIORef 0
    total <- newIORef cap
    vec <- V.unsafeNew cap
    vec' <- newIORef vec
    return $ DynArray used total vec'

appendDyn :: Unbox a => DynArray a -> a -> IO ()
appendDyn (DynArray used total vec) value = do
    used' <- readIORef used
    total' <- readIORef total
    if used' < total'
       then do
           vec' <- readIORef vec
           V.unsafeWrite vec' used' value
           modifyIORef' used (+1)
       else do
           vec' <- readIORef vec
           nvec <- V.unsafeGrow vec' total'
           writeIORef vec nvec
           V.unsafeWrite nvec used' value
           modifyIORef' used (+1)
           modifyIORef' total (*2)

lengthDyn :: DynArray a -> IO Int
lengthDyn (DynArray len _ _) = readIORef len

fetchDyn :: Unbox a => DynArray a -> Int -> IO a
fetchDyn (DynArray _ _ vec) idx = do
    vec' <- readIORef vec
    V.unsafeRead vec' idx

main :: IO ()
main = do
    [n, q] <- (map read . words) <$> getLine
    last <- newIORef 0
    ns <- replicateM n (createDyn 512) :: IO [DynArray Int]
    replicateM_ q $ do
        [k, x, y] <- (map read . words) <$> getLine
        if k == 1
           then do
               a <- readIORef last
               appendDyn (ns !! ((x `xor` a) `mod` n)) y
           else do
               a <- readIORef last
               let vs = ns !! ((x `xor` a) `mod` n)
               len <- lengthDyn vs
               v <- fetchDyn vs (y `mod` len)
               writeIORef last v
               print v

