module Huffman where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BSL
import Data.Word
--import qualified Data.Binary as B

type Map = Map.Map
type CodingTable = Map Char [Bit]

type HuffmanTuple = (Char, [Bit])

data Bit = Zero | One 
    deriving (Eq, Show)

-- #2
data HTree = 
    Node { zeroPath :: HTree, onePath :: HTree, frequency :: Int } |
    Leaf { value :: Char, frequency :: Int }
    deriving (Eq, Show)

node :: HTree -> HTree -> HTree
node zero one = Node { frequency = frequency zero + frequency one, zeroPath = zero, onePath = one }

leaf :: Char -> Int -> HTree
leaf value frequency = Leaf { frequency = frequency, value = value }

-- #3
isConsistent :: HTree -> Bool
isConsistent (Node zero one freq) = isConsistent zero && isConsistent one && freq == frequency zero + frequency one
isConsistent _ = True

-- #4
toCodingTable :: HTree -> CodingTable
toCodingTable tree = Map.fromList $ toCodingMapList tree

toCodingMapList :: HTree -> [HuffmanTuple]
toCodingMapList (Leaf value _) = [(value, [] :: [Bit])]
toCodingMapList (Node zero one _) = 
    prependMapList [Zero] (toCodingMapList zero) ++ 
    prependMapList [One]  (toCodingMapList one )

prependMapList :: [v] -> [(k, [v])] -> [(k, [v])]
prependMapList _ [] = []
prependMapList prefix ((key, value):others) = [(key, prefix ++ value)] ++ (prependMapList prefix others)

-- #5
encode :: CodingTable -> String -> [Bit]
encode _ [] = []
encode table (first:rest) = (table Map.! first) ++ encode table rest

-- #6
decode :: HTree -> [Bit] -> String
decode tree list = decodeImpl tree list tree

decodeImpl :: HTree -> [Bit] -> HTree -> String -- helper function keeping track of the current position in the tree
decodeImpl _ [] (Leaf item _) = [item]
decodeImpl root list (Leaf item _) = item : decodeImpl root list root
decodeImpl root (first:rest) (Node zero one _) = decodeImpl root rest (if first == Zero then zero else one)
decodeImpl _ _ _ = undefined

-- #7
buildHTree :: String -> HTree
buildHTree string = 
    let sorted = List.sortOn snd $ Map.toList $ countChars string
    in buildHTreeImpl [leaf (fst tuple) (snd tuple) | tuple <- sorted]

buildHTreeImpl :: [HTree] -> HTree -- expects the list of HTrees already sorted (ascending frequencies)
buildHTreeImpl [] = undefined
buildHTreeImpl [finished] = finished
buildHTreeImpl (first:second:rest) = buildHTreeImpl $ insertAscending frequency (node first second) rest

countChars :: String -> Map Char Int -- blatantly stolen from https://stackoverflow.com/a/7108655
countChars string = Map.fromListWith (+) [(c, 1) | c <- string]

insertAscending :: Ord b => (a -> b) -> a -> [a] -> [a]
insertAscending _ item [] = [item]
insertAscending func item (first:rest) =
    if func item < func first
        then item : first : rest
        else first : (insertAscending func item rest)

-- #8
toDecodeTree :: CodingTable -> HTree
toDecodeTree table = toDecodeTreeImpl $ Map.toList table

toDecodeTreeImpl :: [HuffmanTuple] -> HTree
toDecodeTreeImpl [] = undefined
toDecodeTreeImpl [(_, (_:_))] = undefined -- error case: leafs are supposed to follow nodes immediately
toDecodeTreeImpl [(char, [])] = leaf char 0
toDecodeTreeImpl (first:rest) =
    let nodes = splitNodes (first:rest)
    in node (toDecodeTreeImpl $ fst nodes) (toDecodeTreeImpl $ snd nodes)

splitNodes :: [HuffmanTuple] -> ([HuffmanTuple], [HuffmanTuple])
splitNodes [] = ([],[])
splitNodes ((_, []):_) = undefined -- can't split if an entry has no bits
splitNodes ((char, (bit:bits)):tuples) =
    let result = splitNodes tuples
        zeroPath = fst result
        onePath = snd result
        entry = (char, bits)
    in if bit == Zero
        then (entry : zeroPath, onePath)
        else (zeroPath, entry : onePath)

eraseFrequency :: HTree -> HTree -- toDecodeTree $ toCodingTable tree == eraseFrequency tree
eraseFrequency (Leaf value _) = leaf value 0
eraseFrequency (Node zero one _) = node (eraseFrequency zero) (eraseFrequency one)

-- #9 
encodeFile :: FilePath -> FilePath -> IO ()
encodeFile = readTransformWrite compressHuffman

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile = readTransformWrite decompressHuffman

readTransformWrite :: (String -> String) -> FilePath -> FilePath -> IO ()
readTransformWrite transform source destination = do 
    input <- readFile source
    let output = transform input
    writeFile destination output

    -- todo: remove these
    putStrLn ("input:  " ++ show input )
    putStrLn ("output: " ++ show output)

compressHuffman :: String -> String
compressHuffman text = 
    let _parseable = toParsableHuffmanData text
    -- todo: how to serialize
    in id text

decompressHuffman :: String -> String
decompressHuffman binary = 
    let _ = 0
    -- todo: how to deserialize
    in id binary


type BinaryData = (
        Int,          -- length
        BS.ByteString -- data
    )

type ParsableHuffmanData = (
        [(Char, BinaryData)], -- head / table
        BinaryData            -- body / data
    )

toParsableHuffmanData :: String -> ParsableHuffmanData
toParsableHuffmanData text = 
    let table = toCodingTable $ buildHTree text -- [(Char, [Bit])]
        bits  = encode table text               -- [Bit]
        head  = [(c, (length b, bitsToByteString b)) | (c, b) <- Map.toList table]
        body  = (length bits, bitsToByteString bits)
    in (head, body)

fromParsableHuffmanData :: ParsableHuffmanData -> String
fromParsableHuffmanData (head, (bodyLength, body)) =
    let bits = byteStringToBits bodyLength body
        table = Map.fromList [(c, byteStringToBits l b) | (c, (l, b)) <- head]
        tree = toDecodeTree table
    in decode tree bits



----------------------- code provided by the professor ----------------------------------------

-- byteStringToBits convert a bytestring to a list of bits. The Int parameter is
-- the expected length of the list. This is required to remove padding bits added
-- by bitsToByteString.
byteStringToBits :: Int -> BS.ByteString -> [Bit]
byteStringToBits n bs =
    let allBits = concatMap fromWord8 (BS.unpack bs)
    in take n allBits -- remove padding bits

-- bitsToByteString converts a list of bits to a ByteString. It pads the last Word8 
-- in the resulting bytestring with zeros.
bitsToByteString :: [Bit] -> BS.ByteString
bitsToByteString topBits = BS.pack (toWord8List topBits)
    where
      toWord8List [] = []
      toWord8List bits =
          let (prefix, suffix) = splitAt 8 bits
          in toWord8 prefix : toWord8List suffix

-- to list of bits must have <= 8 elements
toWord8 :: [Bit] -> Word8
toWord8 bits = go 7 bits
    where
      go :: Int -> [Bit] -> Word8
      go _ [] = 0
      go i (Zero:rest) = go (i - 1) rest
      go i (One:rest) = 2^i + go (i - 1) rest

fromWord8 :: Word8 -> [Bit]
fromWord8 word = go 7 word
    where
      go :: Int -> Word8 -> [Bit]
      go i w
          | i < 0 = []
          | otherwise =
              let x = 2^i
                  this = w `div` x
                  rest = w `mod` x
                  bit = if this == 0 then Zero else One
              in bit : go (i - 1) rest