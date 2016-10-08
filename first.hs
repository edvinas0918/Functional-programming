module First
where

import Data.Char
import Data.List
-- test data ----------------------------------------------
msg0 = "de" -- ()
msg1 = "i46e" -- 46
msg2 = "4:spam" -- spam
msg3 = "d1:v1:x1:x1:21:y1:6e" -- ('v':'x','x':'2','y':'6')
msg4 = "d1:v1:x1:xi2e1:yi6ee" -- ('v':'x','x':2,'y':6)
msg5 = "d1:0d1:v1:x1:xi0e1:yi2ee1:1d1:v1:o1:xi2e1:yi1eee"
msg6 = "d1:0d1:v1:x1:xi0e1:yi1ee1:1d1:v1:o1:xi1e1:yi2ee1:2d1:v1:x1:xi1e1:yi1ee1:3d1:v1:o1:xi0e1:yi0ee1:4d1:v1:x1:xi2e1:yi2ee1:5d1:v1:o1:xi2e1:yi0ee1:6d1:v1:x1:xi0e1:yi2ee1:7d1:v1:o1:xi2e1:yi1ee1:8d1:v1:x1:xi1e1:yi0eee"

-- decoding -----------------------------------------------
bencodeToString :: String -> String
bencodeToString "" = ""
bencodeToString ('i':rest) = 
    let
        (numberString, restString) = readNumberString rest
    in
        numberString

bencodeToString ('d':rest)=
    let
        (dict,restDict) = readDict rest
    in
        dict

bencodeToString string = 
    let
        (str, rest) = readString string
    in
        str

readDict :: String -> (String, String)
readDict string = 
    let
        (str, rest) = readDictItems string 
    in
        ("(" ++ str ++ ")", rest)

readDictItems :: String -> (String, String)
readDictItems "" = error "Dictionary items cannot be empty"
readDictItems ('e' : rest) = ("", rest)
readDictItems string = 
    let 
        (str, rest) = readDictItem string
        (str1, rest1) = readDictItems rest
    in
        case str1 of
            ""          -> (str, rest1)
            otherwise   -> ((str ++ "," ++ str1), rest1)
        

readDictItem :: String -> (String, String)
readDictItem "" = error "Dictionary item cannot be empty"
readDictItem string =
    let
        (firstPart, firstRest) = readDictItemPart string
        (secondPart, secondRest) = readDictItemPart firstRest
    in
        (firstPart ++ ":" ++ secondPart, secondRest)

readDictItemPart :: String -> (String, String)
readDictItemPart ('i':rest) = readNumberString rest
readDictItemPart ('d':rest) = readDict rest
readDictItemPart string = readString string

readString :: String -> (String, String)
readString string =
    let
        stringLengthString = takeWhile (\char -> char /= ':') string
        stringLengthStringLength = length stringLengthString
        stringLengthNumber = read stringLengthString :: Int
        rest = drop (stringLengthStringLength+1) string
        text = take stringLengthNumber rest
        restFinal = drop stringLengthNumber rest
    in
        (("\'" ++ text ++ "\'"), restFinal)

readNumberString :: String -> (String, String)
readNumberString string = 
    let
        numberStr = takeWhile (\char -> char /= 'e') string
        numberStrLength = length numberStr
        rest = drop ((length numberStr)+1) string
    in
        (numberStr, rest)

-- parsing ------------------------------------------------

parseInput :: String -> [(Int, Int, Char)]
parseInput ('(' : rest) = 
    let
        (tuples, rest1) = stringToTuples rest
    in
        tuples
    
parseInput _ = error "Incorrect input"

stringToTuples :: String -> ([(Int, Int, Char)], String)
stringToTuples (',':rest) = stringToTuples rest
stringToTuples (')':rest) = ([], rest)
stringToTuples string =
    let
        (tuple, rest) = stringToTuple string
        (tuples,rest1) = stringToTuples rest
    in
        (tuple:tuples, rest1)

stringToTuple :: String -> ((Int, Int, Char), String)
stringToTuple string =
    let
        (char, rest) = readChar string
        rest1 = skipSymbol rest
    in
        dictToTuple rest1

dictToTuple :: String -> ((Int, Int, Char), String)
dictToTuple ('(':rest) = 
    let
        (temp, rest1) = readChar rest
        rest2 = skipSymbol rest1
        (player, rest3) = readChar rest2
        rest4 = skipSeparator rest3
        (temp1, rest5) = readChar rest4
        rest6 = skipSymbol rest5
        (x, rest7) = readInt rest6
        rest8 = skipSeparator rest7
        (temp2, rest9) = readChar rest8
        rest10 = skipSymbol rest9
        (y, rest11) = readInt rest10
        rest12 = skipSymbol rest11
    in
        ((x, y, player), rest12)

dictToTuple _ = error "Dictionary must start with '('"

readChar :: String -> (Char, String)
readChar ('\'':rest) =
    let
        (h:'\'':rest1) = rest
    in
        (h,rest1)

readInt :: String -> (Int, String)
readInt string = 
    let
        intString = takeWhile (\char -> (ord char) >= 48 && (ord char) <= 57) string
        int = read intString :: Int
        intLength = length intString
        rest = drop intLength string
    in
        (int, rest)

skipSymbol :: String -> String
skipSymbol (_:rest) = rest

skipSeparator :: String -> String
skipSeparator (',':rest) = rest

-- Main algorithm -----------------------------------------

--data Move = Move 
tableSize = 3

availableMoves :: [(Int, Int)]
availableMoves = [
    (0, 0), (0, 1), (0, 2),
    (1, 0), (1, 1), (1, 2),
    (2, 0), (2, 1), (2, 2)]

winConditions :: [[(Int, Int)]]
winConditions = [
    [(0, 0), (0, 1), (0, 2)],
    [(1, 0), (1, 1), (1, 2)],
    [(2, 0), (2, 1), (2, 2)],
    [(0, 0), (1, 0), (2, 0)],
    [(0, 1), (1, 1), (2, 1)],
    [(0, 2), (1, 2), (2, 2)],
    [(0, 0), (1, 1), (2, 2)],
    [(0, 2), (1, 1), (2, 0)]
    ]

test = "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi1e1:yi0ee1:2d1:v1:x1:xi0e1:yi2ee1:3d1:v1:o1:xi1e1:yi2ee1:4d1:v1:x1:xi2e1:yi0ee1:5d1:v1:o1:xi2e1:yi1ee1:6d1:v1:x1:xi0e1:yi1ee1:7d1:v1:o1:xi1e1:yi1eee"
testMoves = parseInput $ bencodeToString test

move :: String -> Maybe (Int, Int, Char)
move str = makeMove $ parseInput $ bencodeToString str

makeMove :: [(Int, Int, Char)] -> Maybe (Int, Int, Char)
makeMove [] = Just (0, 0, 'x')
makeMove moves
    | 9 <= length moves = Nothing
    | not $ isValid moves = Nothing
    | otherwise = Just (x, y, p)
        where
            (x, y) = getAvailableMove moves availableMoves
            p = getPlayer moves

getAvailableMove :: [(Int, Int, Char)] -> [(Int, Int)] -> (Int, Int)
getAvailableMove moves (aMove:aMoves)
    | not $ hasMove moves aMove = aMove
    | otherwise = getAvailableMove moves aMoves

getPlayer :: [(Int, Int, Char)] -> Char
getPlayer moves 
    | mod (length moves) 2 == 0 = 'x'
    | otherwise = 'o'

hasMove :: [(Int, Int, Char)] -> (Int, Int) -> Bool
hasMove [] aMove = False
hasMove ((x, y, _):moves) (x1, y1)
    | x == x1 && y == y1 = True
    | otherwise = hasMove moves (x1, y1)

isSame :: (Int, Int, Char) -> (Int, Int) -> Bool
isSame (a, b, _) (a1, b1) = (a == a1) && (b == b1)

isValid :: [(Int, Int, Char)] -> Bool
isValid moves
    | length moves < 5 = True
    | otherwise =
        (isValid' movesX winConditions) && (isValid' movesY winConditions) 
            where
                movesX = getMoves moves 'x'
                movesY = getMoves moves 'o'
                isValid' moves' [] = True
                isValid' moves' (winC:winConds)
                    | hasMoves moves' winC = False
                    | otherwise = isValid' moves' winConds

getMoves :: [(Int, Int, Char)] -> Char -> [(Int, Int)]
getMoves moves player =
    getMoves' [] moves player
    where
        getMoves' :: [(Int, Int)] -> [(Int, Int, Char)] -> Char -> [(Int, Int)]
        getMoves' playerMoves [] player = playerMoves
        getMoves' playerMoves ((x,y,p):moves') player
            | p == player = getMoves' ((x,y):playerMoves) moves' player
            | otherwise = getMoves' playerMoves moves' player

hasMoves :: [(Int, Int)] -> [(Int, Int)] -> Bool
hasMoves playerMoves [] = True
hasMoves playerMoves (winM:winMoves)
    | not $ elem winM playerMoves = False
    | otherwise = hasMoves playerMoves winMoves


--makeMove [] = Just (0, 0, x)

--getAvailableMove :: [(Int, Int, Char)] -> Int -> Int -> (Int, Int, Char)
--getAvailableMove moves (aMove:aMoves)
