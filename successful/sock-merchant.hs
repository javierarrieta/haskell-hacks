

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
-- import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


myFold :: Int -> (Set Int, Int) -> (Set Int, Int)
myFold a  (origSet, count)
  |  member a origSet = (Data.Set.delete a origSet, count + 1)
  | otherwise         = (Data.Set.insert a origSet, count)



-- Complete the sockMerchant function below.
sockMerchant :: Int -> [Int] -> Int
sockMerchant n ar = snd(Data.List.foldr myFold (Data.Set.empty, 0) ar)