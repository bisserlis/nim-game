import qualified Data.Bits   as Bit
import qualified Data.Map    as Map
import           Data.List
import           Data.Word   (Word64, Word8) -- 64 bit unsigned integer
import           Data.Maybe
import           Data.Char
import           Data.List.Utils (replace)


data Player = Human
            | Computer
            deriving (Eq, Show, Bounded)

instance Enum Player where
  succ Human    = Computer
  succ Computer = Human

  fromEnum Human    = 0
  fromEnum Computer = 1

  toEnum 0 = Human
  toEnum 1 = Computer

data GameState = Game { player :: Player, position :: NimPosition }

instance Show GameState where
  show (Game Human position)    = "Computer's play => " ++
                                  show position ++
                                  "\n" ++
                                  "Your turn    ====> "
  show (Game Computer position) = ""
  
data Bit = Bit Bool
         deriving (Eq, Ord)

toBit 0 = Bit False
toBit _ = Bit True

instance Show Bit where
  show (Bit False) = "0"
  show (Bit True ) = "1"

data Binary = Binary [Bit]
            deriving (Eq, Ord)

toBitList :: Integral a => a -> [Bit]
toBitList 0 = []
toBitList n = let (q, r) = n `divMod` 2
              in (toBit r) : toBitList q

toBinary :: Integral a => a -> Binary
toBinary n = (Binary . toBitList) n

instance Show Binary where
  show (Binary bitList) = concat $ (map show) . reverse $ bitList

data NimPosition = NimPosition (Map.Map Word64 Word64)
                 deriving (Eq)
-- A NimPosition is constructed from a map from Word64 to Word64
--The keys correspond to the distinct pile sizes, and the values correspond to
--the number of piles with that size.

instance Show NimPosition where
  show position =
    let nimPileList             = (show . toList) position
        commaSeparatedPileSizes = (init . tail  ) nimPileList
        spaceSeparatedPileSizes = replace "," " " commaSeparatedPileSizes
    in spaceSeparatedPileSizes


insertWithCounts :: Word64 -> Map.Map Word64 Word64 -> Map.Map Word64 Word64
-- Insert an Word64 into a map as a key. If that Word64 is already present
-- in the map as a key, then increase the value by 1. If the Word64 is
-- not already present, give it the default value of 1.
insertWithCounts pileSize oldMap = Map.insertWith (\_ y -> y + 1)
                                                  pileSize
                                                  1     -- default value
                                                  oldMap        

toList :: NimPosition -> [Word64]
-- Convert a NimPosition into a list of Word64, where each Word64 in the list
-- corresponds to a pile.
toList (NimPosition position) = 
    let pileSizes = Map.keys  position
        pileQtys  = Map.elems position
        pileLists = zipWith replicate (map fromIntegral pileQtys) pileSizes
    in foldr1 (++) pileLists

fromList :: [Word64] -> NimPosition
-- Construct a NimPosition from a list of Word64, where each Word64 is a
-- pile.
fromList xs = NimPosition (foldr insertWithCounts Map.empty xs)

positionSum :: NimPosition -> Word64
-- Compute the bitwise xor of the pile sizes.
positionSum position = foldr1 (Bit.xor) (toList position)
                       
winning :: NimPosition -> Bool
-- According to Bouton's theorem, a position in nim is winning if the
-- bitwise exclusive or of the pile sizes is exactly zero.
winning position = (positionSum position == 0)

losing :: NimPosition -> Bool
losing position = (sum . toList) position == 1

findNumWithLeadingBit :: [Word64] -> Maybe Word64
findNumWithLeadingBit xs
    | maxBinaryLengthIsUnique = lookup maxBinaryLength lengthValueAlist
    | otherwise               = Nothing
    where binaryExpansions        = map (show . toBinary) xs
          binaryLengths           = map length binaryExpansions
          lengthValueAlist        = zip binaryLengths xs
          maxBinaryLength         = maximum binaryLengths
          numsWithMaxBinaryLength = filter (== maxBinaryLength) binaryLengths
          maxBinaryLengthIsUnique = length numsWithMaxBinaryLength == 1

isValidMove :: NimPosition -> NimPosition -> Bool
isValidMove prevPosition nextPosition =
  let prevPiles              = toList prevPosition
      nextPiles              = toList nextPosition
      pilesNotInPrevPosition = nextPiles \\ prevPiles
      pilesNotInNextPosition = prevPiles \\ nextPiles
  in case (pilesNotInNextPosition, pilesNotInPrevPosition) of
      (originalSize:[],resultantSize:[]) | resultantSize < originalSize -> True
                                         | otherwise                    -> False
      _ -> False

nextMove :: NimPosition -> NimPosition
nextMove prevPosition =
  if winning prevPosition then
    let prevList = (reverse . toList) prevPosition
        nextList = (head prevList - 1) : (tail prevList)
    in fromList nextList
  else
  let prevList = toList prevPosition
  in case findNumWithLeadingBit prevList of
    Just bigPile -> fromList (newPile:otherPiles)
      where otherPiles = delete bigPile prevList
            newPile    = foldr1 (Bit.xor) otherPiles

    Nothing -> head possibleMoves
      where remainingPiles   = zipWith delete prevList (repeat prevList)
            remainingNimSums = map (foldr1 Bit.xor) remainingPiles
            candidateLists   = zipWith (:) remainingNimSums remainingPiles
            candidateMoves   = map fromList candidateLists
            possibleMoves    = filter (isValidMove prevPosition) candidateMoves


readIntListFromString :: String -> [Word64]
readIntListFromString input = case readIntFromString input of
  (Nothing, _)              -> []
  (Just intRead, remainder) -> intRead : (readIntListFromString remainder)


readIntFromString :: String -> (Maybe Word64, String)
readIntFromString string =
  let (_, newString)         = span (isSpace) string
      (intString, remainder) = span (isNumber) newString
      numberRead             = case null intString of
                                True  -> Nothing
                                False -> Just (read intString)
  in (numberRead, remainder)


getIntList :: IO [Word64]
getIntList = do
  line <- getLine
  let intListRead = readIntListFromString line in
   case null intListRead of
    True  -> do
      putStrLn "Parse error: can't read list of integers"
      getIntList
    False -> return intListRead

getNimPosition :: IO NimPosition
getNimPosition = do
  intList <- getIntList
  return $ fromList intList

getValidNimPosition :: NimPosition -> IO NimPosition
getValidNimPosition oldPosition = do
  newPosition <- getNimPosition
  case isValidMove oldPosition newPosition of
   False -> do
     putStrLn "Player error: not a valid position"
     getValidNimPosition oldPosition
   True  -> return newPosition
  

takeTurns :: Maybe GameState -> IO (Maybe GameState)
takeTurns Nothing = do putStrLn "Game Over!"; return Nothing
takeTurns (Just currentState) = do
  (putStr . show) currentState
  case losing $ position currentState of
    True -> do putStrLn "Game over!"
               return Nothing
    _ ->
      case player currentState of
       Computer ->
         let computersNextMove = nextMove $ position currentState
             nextState = currentState { player   = Human,
                                        position = computersNextMove}
         in do takeTurns $ Just nextState
       Human -> do
         playersNextMove <- getValidNimPosition $ position currentState
         let nextState = currentState { player   = Computer,
                                        position = playersNextMove} 
           in do takeTurns $ Just nextState

main = do
  putStr "Initial position => "
  startingPosition <- getNimPosition
  let initialGameState = Just Game { player = Computer,
                                     position = startingPosition }
    in takeTurns initialGameState
