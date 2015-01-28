import qualified Data.Bits as Bit
import qualified Data.Map  as Map
import Data.Word       (Word64)
import Data.List       
import Data.Char

data NimPosition = NimPosition (Map.Map Word64 Word64)
                 deriving (Eq)
-- A NimPosition is constructed from a map from Word64 to Word64. The
-- keys correspond to the distinct pile sizes, and the values
-- correspond to the number of piles with that size.

data Player = Human
            | Computer

data GameState = Game
                 { player   :: Player
                 , position :: NimPosition }

data Bit = Bit Bool
         deriving (Eq, Ord)

data Binary = Binary [Bit]
            deriving (Eq, Ord)



insertWithCounts :: Word64
                 -> Map.Map Word64 Word64
                 -> Map.Map Word64 Word64
-- Insert an Word64 into a map as a key. If that Word64 is already present
-- in the map as a key, then increase the value by 1. If the Word64 is
-- not already present, give it the default value of 1.
insertWithCounts pileSize oldMap =
  Map.insertWith (\_ y -> y + 1) pileSize 1 oldMap        


fromList :: [Word64] -> NimPosition
-- Construct a NimPosition from a list of Word64, where each Word64 is a
-- pile.
fromList xs = NimPosition (foldr insertWithCounts Map.empty xs)



toList :: NimPosition -> [Word64]
-- Convert a NimPosition into a list of Word64, where each Word64 in the list
-- corresponds to a pile.
toList (NimPosition position) = 
    let pileSizes = Map.keys  position
        pileQtys  = Map.elems position
        pileLists = zipWith replicate (map fromIntegral pileQtys) pileSizes
    in foldr1 (++) pileLists



instance Show NimPosition where
  show =
    unwords . map show . toList


instance Show GameState where
  show (Game Human position) = "Computer's play....=> " ++ show position ++ "\n"
                               ++ "Your turn..........=> "
  show (Game Computer position) = ""
  

toBit 0 = Bit False
toBit _ = Bit True

instance Show Bit where
  show (Bit False) = "0"
  show (Bit True ) = "1"


toBitList :: Integral a => a -> [Bit]
toBitList 0 = []
toBitList n = let (q, r) = n `divMod` 2
              in (toBit r) : toBitList q

toBinary :: Integral a => a -> Binary
toBinary n = (Binary . toBitList) n

instance Show Binary where
  show (Binary bitList) = concat $ (map show) . reverse $ bitList


positionSum :: NimPosition -> Word64
-- Compute the bitwise xor of the pile sizes.
positionSum position = foldr1 (Bit.xor) (toList position)
                       
winning :: NimPosition -> Bool
-- According to Bouton's theorem, a position in nim is winning if the
-- bitwise exclusive or of the pile sizes is exactly zero.
winning position = (positionSum position == 0)


losing :: NimPosition -> Bool
losing position = (sum . toList) position == 1

terminal :: NimPosition -> Bool
terminal position = (sum . toList) position == 0

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
takeTurns (Just currentState) =
  let currentPosition = position currentState in
  do (putStr . show) currentState
     case (losing currentPosition) || (terminal currentPosition) of
      True -> takeTurns Nothing
      _ ->
        case player currentState of
         Computer ->
           let computersNextMove = nextMove $ position currentState
               nextState = currentState { player   = Human,
                                       position = computersNextMove}
           in takeTurns $ Just nextState
         Human -> do
           playersNextMove <- getValidNimPosition $ position currentState
           let nextState = currentState { player   = Computer
                                        , position = playersNextMove} in do
             takeTurns $ Just nextState

data YesNo = Yes | No

getYesOrNo :: IO (YesNo)
getYesOrNo = do
  input <- getLine
  case input of
   "yes" -> return Yes
   "y"   -> return Yes
   "no"  -> return No
   "n"   -> return No
   _     -> do putStr "Please enter 'yes' or 'no': "; getYesOrNo


introduceGame :: IO ()
introduceGame = putStrLn
  "Welcome to Nim! To get started, enter your initial position, e.g. '1 3 5'"
                

main = do
  introduceGame
  putStr "Initial position   => "
  startingPosition <- getNimPosition
  let initialGameState = Just Game { player = Computer,
                                     position = startingPosition }
    in takeTurns initialGameState
  putStr "Would you like to continue? (y/n): "
  shouldContinue <- getYesOrNo
  case shouldContinue of
   Yes -> main
   No  -> do putStrLn "Goodbye!"; return ()
