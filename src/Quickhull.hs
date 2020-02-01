{-# LANGUAGE TypeOperators #-}

module Quickhull
    ( quickhull
    , Point
    , propagateR
    , segmentedPostscanr
    ) where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Unsafe as Unsafe
import qualified Prelude as P

-- Accelerate backend
import Data.Array.Accelerate.Interpreter
-- import Data.Array.Accelerate.LLVM.Native
-- import Data.Array.Accelerate.LLVM.PTX

type Point = (Int, Int)

type Line = (Point, Point)

type SegmentedPoints = (Vector Bool, Vector Point)

pointIsLeftOfLine :: Exp Line -> Exp Point -> Exp Bool
pointIsLeftOfLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y > c
  where
    nx = y1 - y2
    ny = x2 - x1
    c = nx * x1 + ny * y1

nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Int
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c = nx * x1 + ny * y1

-- * Exercise 1
leftMostPoint :: Acc (Vector Point) -> Acc (Scalar Point)
leftMostPoint = fold min (T2 maxBound maxBound)

rightMostPoint :: Acc (Vector Point) -> Acc (Scalar Point)
rightMostPoint = fold max (T2 minBound minBound)

initialPartition :: Acc (Vector Point) -> Acc SegmentedPoints
initialPartition points =
  let
    p1 = the $ leftMostPoint points
    p2 = the $ rightMostPoint points
    line = T2 p1 p2
    doReverse (T2 _ y1) (T2 _ y2) = y1 > y2
    -- * Exercise 2
    isUpper :: Acc (Vector Bool)
    isUpper = ifThenElse (doReverse p1 p2) (map isUpperRev points) (map isUpperNoRev points)

    --The normal version of isUpper. use this for a line with a positive increment
    isUpperNoRev:: Exp Point -> Exp Bool
    isUpperNoRev point = ifThenElse (point == p1 || point == p2) (constant False) (pointIsLeftOfLine line point)

    --The reverse version of isUpper, use this for a line with a negative increment
    isUpperRev:: Exp Point -> Exp Bool
    isUpperRev point = ifThenElse (point == p1 || point == p2) (constant False) (not $ pointIsLeftOfLine line point)

    isLower :: Acc (Vector Bool)
    isLower = ifThenElse (doReverse p1 p2) (map isUpperNoRev points) (map isUpperRev points) --Because lower is exactly the opposite of isUpper, we can swap the order of isUpper and resuse these functions

    -- * Exercise 3
    lowerIndices :: Acc (Vector Int)
    lowerIndices = prescanl (+) 0 (map boolToInt isLower)

    -- * Exercise 4
    upperIndices :: Acc (Vector Int)
    upperIndices = prescanl (+) 0 (map boolToInt isUpper)
    
    countUpper :: Acc (Scalar Int)
    countUpper = sum (map (boolToInt) isUpper)

    -- * Exercise 5
    permutation :: Acc (Vector (Z :. Int))
    permutation =
      let
        f :: Exp Point -> Exp Bool -> Exp Int -> Exp Int -> Exp (Z :. Int)
        f p upper idxLower idxUpper
          = ifThenElse (p == p1) (index1 0) (ifThenElse (p == p2) (index1 ((the $ countUpper) + 1)) (ifThenElse (upper) (index1 (idxUpper+1)) (index1 ((the $ countUpper) + idxLower + 2))))
      in
        zipWith4 f points isUpper lowerIndices upperIndices

    -- * Exercise 6
    empty :: Acc (Vector Point)
    empty = generate (index1 ((size points) +1)) (\_ -> p1)

    newPoints :: Acc (Vector Point)
    newPoints = permute const empty (permutation !) points

    -- * Exercise 7
    headFlags :: Acc (Vector Bool)
    headFlags = fill (index1 1) (constant True) ++ fill (index1 (the countUpper)) (constant False) ++ fill (index1 1) (constant True) ++ fill (index1 ((size newPoints)- 3 - (the countUpper))) (constant False) ++ fill (index1 1) (constant True)
  in
    T2 headFlags newPoints

-- * Exercise 8
segmentedPostscanl :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedPostscanl f headFlags vec = map snd $ postscanl (segmentedL f) (Unsafe.undef) (zip headFlags vec)

segmentedPostscanr :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedPostscanr f headFlags vec = map snd $ postscanr (segmentedR f) (Unsafe.undef) (zip headFlags vec)

--The segmented left version of a function f
segmentedL :: Elt a => (Exp a -> Exp a -> Exp a) -> (Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a))
segmentedL op (T2 fx x) (T2 fy y) = T2 ( fx || fy ) ( fy ? (y, op x y) )

--The segmented right version of a function f
segmentedR :: Elt a => (Exp a -> Exp a -> Exp a) -> (Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a))
segmentedR op (T2 fx x) (T2 fy y) = T2 ( fx || fy ) ( fx ? (x, op x y) )

-- * Exercise 9
propagateL :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateL = segmentedPostscanl const

propagateR :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateR = segmentedPostscanr (P.flip const)

-- * Exercise 10
propagateLine :: Acc SegmentedPoints -> Acc (Vector Line)
propagateLine (T2 headFlags points) = zip vecP1 vecP2
  where
    vecP1 = propagateL headFlags points
    vecP2 = propagateR headFlags points

-- * Exercise 11
shiftHeadFlagsL :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsL flags = tail flags ++ fill (index1 1) (constant False)
--                      All items except the first + a False

shiftHeadFlagsR :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsR flags = fill (index1 1) (constant False) ++ init flags
--                      A flase + all items except the last

partition :: Acc SegmentedPoints -> Acc SegmentedPoints
partition (T2 headFlags points) =
  let
    vecLine :: Acc (Vector Line)
    vecLine = propagateLine (T2 headFlags points)

    headFlagsL = shiftHeadFlagsL headFlags
    headFlagsR = shiftHeadFlagsR headFlags

    -- * Exercise 12
    furthest :: Acc (Vector Point)
    furthest = propagateR headFlagsL (map fst $ (segmentedPostscanl getFurtherPoint headFlags (zip points vecLine)))

    getFurtherPoint :: Exp (Point, Line) -> Exp (Point, Line) -> Exp (Point, Line)
    getFurtherPoint (T2 prevPoint _) (T2 currPoint line) = ifThenElse ((nonNormalizedDistance line prevPoint) > (nonNormalizedDistance line currPoint)) (T2 prevPoint line) (T2 currPoint line)

    -- * Exercise 13
    isLeft :: Acc (Vector Bool)
    isLeft = zipWith3 combinationFunctionLeft points furthest vecLine

    --Create a new line from p1 to furthest, check if testPoint lies on the left of this line
    combinationFunctionLeft :: Exp Point -> Exp Point -> Exp Line -> Exp Bool
    combinationFunctionLeft testPoint furthest (T2 p1 _) = pointIsLeftOfLine (T2 p1 furthest) testPoint

    isRight :: Acc (Vector Bool)
    isRight = zipWith3 combinationFunctionRight points furthest vecLine

    --Create a new line from p2 to furthest, check if testPoint lies on the right of this line
    combinationFunctionRight :: Exp Point -> Exp Point -> Exp Line -> Exp Bool
    combinationFunctionRight testPoint furthest (T2 _ p2) = pointIsRightOfLine (T2 p2 furthest) testPoint

    --Returns true if point is on the right of the line
    pointIsRightOfLine :: Exp Line -> Exp Point -> Exp Bool
    pointIsRightOfLine line point = (not (pointIsLeftOfLine line point) && (not (nonNormalizedDistance line point == 0)))

    -- * Exercise 14
    segmentIdxLeft :: Acc (Vector Int)
    segmentIdxLeft = segmentedPostscanl (+) headFlags (map boolToInt isLeft)

    segmentIdxRight :: Acc (Vector Int)
    segmentIdxRight = segmentedPostscanl (+) headFlags (map boolToInt isRight)

    -- * Exercise 15
    countLeft :: Acc (Vector Int)
    countLeft = propagateR headFlagsL segmentIdxLeft
    
    -- * Exercise 16                
    segmentSize :: Acc (Vector Int) 
    segmentSize = zipWith segHelperFlags headFlags (zipWith3 segHelperVals headFlagsL segmentIdxLeft segmentIdxRight)

    --Insert the segment size values
    segHelperVals :: Exp Bool -> Exp Int -> Exp Int -> Exp Int
    segHelperVals hf idL idR = ifThenElse hf (idL + idR + 1) 0

    --Insert the 1's for the headFlags, otherwise keep the previous value
    segHelperFlags :: Exp Bool -> Exp Int -> Exp Int
    segHelperFlags headFlag currVal = ifThenElse headFlag 1 currVal

    segmentOffset :: Acc (Vector Int)
    size :: Acc (Scalar Int) --Remove the last value, due to inclusive scan     --The last result of the array is the total size
    T2 segmentOffset size = T2 (take ((length scanResult)-1) scanResult) (unit (scanResult !! ((length scanResult)-1)))
     where scanResult = scanl (+) 0 segmentSize

    -- * Exercise 17
    permutation :: Acc (Vector (Z :. Int))
    permutation =
      let
        f :: Exp Bool -> Exp Point -> Exp Point -> Exp Bool -> Exp Bool -> Exp Int -> Exp Int -> Exp Int -> Exp Int -> Exp (Z :. Int)
        f flag p furthestP left right offset cntLeft idxLeft idxRight
          = condA
          where condA = ifThenElse (not flag && not (p == furthestP) && not left && not right) ignore condB --Decide which to ignore (for some reason we had to put not in front of each one instead of 1 infront of the big thing)
                condB = ifThenElse flag (index1 offset) (condC) --Put the headflags at the segOffset
                condC = ifThenElse (p == furthestP) (index1 (offset + cntLeft)) condD -- put the furthest point at segOffset + cntLeft
                condD = ifThenElse left (index1 (idxLeft + offset - 1)) condE -- put a left value at leftIdx + segOffset - 1
                condE = ifThenElse right (index1 (cntLeft + idxRight + offset)) Unsafe.undef --It should never be able to reach the undef
      in
        zipWith9 f headFlags points furthest isLeft isRight segmentOffset countLeft segmentIdxLeft segmentIdxRight

    -- * Exercise 18
    empty :: Acc (Vector Point)
    empty = fill (index1 (the $ size)) Unsafe.undef

    newPoints :: Acc (Vector Point)
    newPoints = permute const empty (permutation !) points

    -- * Exercise 19
    newHeadFlags :: Acc (Vector Bool)
    newHeadFlags = permute const baseArray (permutation !) sourceValues
     where sourceValues = zipWith3 mergeFunc furthest points headFlags
           baseArray = fill (index1 (the $ size)) (constant False)
    
    mergeFunc :: Exp Point -> Exp Point -> Exp Bool -> Exp Bool
    mergeFunc furthestPoint point flag = ifThenElse flag (constant True) (point == furthestPoint)
 
  in
    T2 newHeadFlags newPoints

-- * Exercise 20
condition :: Acc SegmentedPoints -> Acc (Scalar Bool)
condition x = fold (||) (constant False) (afst x) -- The Fold will only be True if there's no False value left

-- * Exercise 21
quickhull' :: Acc (Vector Point) -> Acc (Vector Point)
quickhull' x = asnd $ awhile condition partition (initialPartition x)

quickhull :: Vector Point -> Vector Point
quickhull = run1 quickhull'

-- * Bonus
quickhullSort' :: Acc (Vector Int) -> Acc (Vector Int)
quickhullSort' = undefined

quickhullSort :: Vector Int -> Vector Int
quickhullSort = run1 quickhullSort'
