module Parse.S19 (parse) where
import Data.List (sort, sortBy)
import Data.Char (toLower)

-- https://en.eeworld.com.cn/news/mcu/eic543348.html

hexToInt :: Char -> Int
hexToInt c =
  case toLower c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'a' -> 10
    'b' -> 11
    'c' -> 12
    'd' -> 13
    'e' -> 14
    'f' -> 15
    _   -> error $ [c] ++ " is not a hexadecimal digit!"

twoHexToInt :: Char -> Char -> Int
twoHexToInt l r = (16 * hexToInt l) + hexToInt r

hexBytesToInts :: [Char] -> [Int]
hexBytesToInts [] = []
hexBytesToInts (_:[]) = error "Can't convert a list with an odd amount of chars to ints"
hexBytesToInts (l:r:rest) = (twoHexToInt l r):(hexBytesToInts rest)

type Record = String
type Address = Int
type Type = Int
type Data = [Int]

readRecord :: Record -> (Type, Address, Data)
readRecord record@('S':n':_:_:rest) =
  let
    n = hexToInt n'
  in
    case n of
      1 ->
        let
          (adr, bytes) = case rest of
            _:_:adr1:adr0:bytes' -> (twoHexToInt adr1 adr0, hexBytesToInts $ init $ init bytes')
            _ -> error $ "Incorrectly formatted record! Record: " ++ record
        in
          (n, adr, bytes)
      9 ->
        let
          adr = case rest of
            _:_:adr1:adr0:_ -> twoHexToInt adr1 adr0
            _ -> error $ "Incorrectly formatted record! Record: " ++ record
        in
          (n, 0xFF, [adr])
      _ -> error $ "S" ++ [n'] ++ "is not a supported S-record!"

readRecord record = error $ "Incorrect format for S-record! Record: " ++ record

applyRecord :: Data -> (Address, Data) -> Data
applyRecord memory (start, recordData) =
  let
    end = length recordData
    (first, second') = splitAt start memory
    second = snd $ splitAt end second'
  in
    first ++ recordData ++ second

parse :: [Record] -> [Int]
parse records' =
  let
    records = filter (\ record -> not $ null record) records'
    recordData = map readRecord records
    sorted = map (\ (_, adr, bytes) -> (adr, bytes)) $ sortBy (\ (ln, _, _) (rn, _, _) -> rn `compare` ln) recordData

    memoryZero = replicate 256 0
  in
    foldl (\ acc record -> applyRecord acc record) memoryZero sorted
