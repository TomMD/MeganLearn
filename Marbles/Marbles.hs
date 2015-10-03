{-# LANGUAGE BinaryLiterals #-}
import Prelude hiding (Either(..))
import Data.Word (Word64)
import Data.Maybe (catMaybes)
import Data.Bits (testBit,setBit,clearBit,popCount)
import qualified Data.Set as Set
import Graphics.Gloss.Interface.Pure.Animate

main :: IO ()
main = animate (InWindow "Marbles" (600,600) (0, 0) ) white renderFrame

--------------------------------------------------------------------------------
--  Board Abstraction

newtype MarbleBoard = MB Word64 deriving (Eq,Ord,Show)

(!) :: MarbleBoard -> (Int,Int) -> Bool
(MB b) ! p = testBit b (pointToBitIndex p)

pointToBitIndex :: (Int,Int) -> Int
pointToBitIndex (x,y) = y*7 + x

validPoint :: (Int,Int) -> Bool
validPoint p@(x,y) = validPointBoard ! p && x < 7 && y < 7 && x >= 0 && y >= 0
 where validPointBoard = MB 0b0011100001110011111111111111111111100111000011100

initialBoard :: MarbleBoard
initialBoard = MB 0b0011100001110011111111110111111111100111000011100

peg,noPeg :: MarbleBoard -> (Int,Int) -> Bool
peg     = (!)
noPeg b = not . (b !)

jumpMb :: MarbleBoard -> (Int,Int) -> Direction -> Maybe MarbleBoard
jumpMb mb@(MB b) src@(x,y) dir
    | peg mb src && good = Just $ MB (setBit (clearBit (clearBit b srcIx) midIx) dstIx)
    | otherwise          = Nothing
 where
  srcIx     = pointToBitIndex src
  dstIx     = pointToBitIndex dst
  midIx     = pointToBitIndex mid
  (mid,dst,good) =
        let (m,d) = case dir of
                        Up    -> ((x,y-1), (x,y-2))
                        Down  -> ((x,y+1), (x,y+2))
                        Left  -> ((x-1,y), (x-2,y))
                        Right -> ((x+1,y), (x+2,y))
        in (m,d, peg mb m && noPeg mb d && validPoint d)

data Direction = Up | Down | Left | Right deriving (Eq,Ord,Show,Enum,Bounded,Read)

jumps :: MarbleBoard -> (Int,Int) -> [MarbleBoard]
jumps mb pnt = catMaybes $ map (jumpMb mb pnt) [Up .. Right]

allJumps :: MarbleBoard -> [MarbleBoard]
allJumps mb = concatMap (jumps mb) [(x,y) | (x,y) <- allPos]

allPos :: [(Int,Int)]
allPos = [(x,y) | x <- [0..6], y <- [0..6]] -- Over-approximation of all positions.

won :: MarbleBoard -> Bool
won m@(MB b) = popCount b == 1 && m ! (3,3)

--------------------------------------------------------------------------------
--  Solver Logic

-- Yields a list of boards representing a winning game (last board first)
solveMarbles :: [MarbleBoard]
solveMarbles = go Set.empty [j : [initialBoard] | j <-  allJumps initialBoard]
 where
  -- Set of observed states -> List of possible sequences of boards (never empty) -> Winning sequence of boards (or empty)
  go _ [] = []
  go s (curr@(b:_):rest)
    | won b     = curr
    | otherwise =
        let nexts = filter (`Set.notMember` s) [j : curr | j <- allJumps b]
        in go (Set.union (Set.fromList nexts) s) $ nexts ++ rest

--------------------------------------------------------------------------------
--  Rendering

renderFrame :: Float -> Picture
renderFrame time = render board
 where
  board = reverse solveMarbles !! idx
  idx   = min (length solveMarbles - 1) (floor (time / frameTime))

frameTime :: Float
frameTime = 1.5

render :: MarbleBoard -> Picture
render b = pictures $ emptyBoard : pieces
 where
 pieces = [renderPiece (x,y) | (x,y) <- allPos ]
 renderPiece pos
    | noPeg b pos = blank
    | otherwise   = moveOnGrid pos (color black (thickCircle 1 30))

moveOnGrid :: (Int,Int) -> Picture -> Picture
moveOnGrid (x,y) p = translate (fromIntegral x * spacing) (fromIntegral y * spacing) p
 where spacing = 40

emptyBoard :: Picture
emptyBoard = pictures [if validPoint p then moveOnGrid p (color yellow (thickCircle 1 30)) else blank | p <- allPos]
