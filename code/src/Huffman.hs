module Huffman where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BSL
import Data.Word
-- import qualified Data.Binary as B

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
encodeFile srcFile dstFile = do
    print "encodeFile"
    print srcFile
    print dstFile

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile srcFile dstFile = do
    print "decodeFile"
    print srcFile
    print dstFile

bitToBool :: Bit -> Bool
bitToBool bit = if bit == Zero then False else True

boolToBit :: Bool -> Bit
boolToBit bool = if bool == False then Zero else One

data ParsableHuffmanData = HuffmanEncoding {
        primitiveTable :: [(Char, [Bool])],
        bitCount :: Int,
        bitSequence :: [Word8]
    }

compressMap :: Map Char [Bit] -> [(Char, [Bool])]
compressMap map = [(char, [bitToBool b | b <- bits]) | (char, bits) <- (Map.toList map)]

--decompressMap :: [(Char, [Bool])] -> Map Char [Bit]
--decompressMap list = Map.fromList [|]

{-
huffmanPack :: String -> ParsableHuffmanData
huffmanPack message = 
    let tree = buildHTree message
        table = toCodingTable tree
        encodeMessage = encode table message
    in ParsableHuffmanData

huffmanUnpack :: ParsableHuffmanData -> String
huffmanPack _data = undefined


huffmanPack :: (CodingTable -> String) -> ([Bit] -> String) -> String -> String
huffmanPack serializeTable serializeBits message =
    let tree = buildHTree message
        table = toCodingTable tree
        encodedMessage = serializeBits (encode table message)
        encodedTable = serializeTable table
    in encodedTable ++ encodedMessage

huffmanUnpack :: (String -> CodingTable) -> (String -> [Bit]) -> String -> String
huffmanUnpack deserializeTable deserializeBits encoded =
    let table =
-}