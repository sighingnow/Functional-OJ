{-# OPTIONS_GHC -O2 -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

-- Codeforces 547B

import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.ByteString.Char8             as BC
import           Data.IORef
import qualified Data.List                         as L
import           Data.Maybe

import           Data.Vector.Fusion.Stream.Monadic

import qualified Data.Vector.Unboxed.Mutable       as Vec

-- history:
-- 1. 2015-09-23: finish this solution, but time limit exceeded.
-- 2. 2015-09-26: re-write this solution using `Data.Vertor`, but compilation error, said...

-- For each i, find the largest j that aj
