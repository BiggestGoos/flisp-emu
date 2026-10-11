module Parse.S19 (parse) where
import Data.List (sortBy)

import Utils

-- https://en.eeworld.com.cn/news/mcu/eic543348.html

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
