module Main where

import Huffman
import Test.HUnit
import System.Exit
import qualified Data.Map.Strict as Map

-- concrete testing values

-- #2
automaticSampleTree :: HTree
automaticSampleTree = node 
    (node (leaf 'E' 158) (node (leaf 'N' 97) (leaf 'I' 82)))
    (node (node (leaf 'R' 77) (leaf 'S' 67)) (node (leaf 'T' 64) (leaf 'A' 61)))

manualSampleTree :: HTree
manualSampleTree = Node 
  (Node (Leaf 'E' 158) (Node (Leaf 'N' 97) (Leaf 'I' 82) 179) 337)
  (Node (Node (Leaf 'R' 77) (Leaf 'S' 67) 144) (Node (Leaf 'T' 64) (Leaf 'A' 61) 125) 269)
  606

expectedZeroFrequencyTree :: HTree
expectedZeroFrequencyTree = node 
    (node (leaf 'E' 0) (node (leaf 'N' 0) (leaf 'I' 0)))
    (node (node (leaf 'R' 0) (leaf 'S' 0)) (node (leaf 'T' 0) (leaf 'A' 0)))

expectedBitSequence :: [Bit]
expectedBitSequence = [Zero, Zero, Zero, One, Zero, One, One, Zero, Zero, Zero]

expectedCodingMapList :: [(Char, [Bit])]
expectedCodingMapList = [
    ('E', [Zero, Zero]),
    ('N', [Zero, One , Zero]),
    ('I', [Zero, One , One ]),
    ('R', [One , Zero, Zero]),
    ('S', [One , Zero, One ]),
    ('T', [One , One , Zero]),
    ('A', [One , One , One ])
  ]

expectedCodingTable :: CodingTable
expectedCodingTable = Map.fromList expectedCodingMapList

-- tests

reportNotImplemented :: String -> IO ()
reportNotImplemented name = assertEqual (">>> test " ++ name ++ " not implemented! <<<") False True

test_isConsistent :: IO ()
test_isConsistent = do
  assertEqual "automatic" True (isConsistent automaticSampleTree)
  assertEqual "manual" True (isConsistent manualSampleTree)
  assertEqual "equality" manualSampleTree automaticSampleTree
  pure ()

test_toCodingTable :: IO ()
test_toCodingTable = do
  assertEqual "codingTable" expectedCodingTable (toCodingTable automaticSampleTree)
  pure ()

test_encode :: IO ()
test_encode = do
  assertEqual "encode" expectedBitSequence (encode expectedCodingTable "ENTE")
  pure ()

test_decode :: IO ()
test_decode = do
  assertEqual "decode" "ENTE" (decode manualSampleTree expectedBitSequence)
  pure ()

test_buildHTree :: IO ()
test_buildHTree = do
  let testString = "die keksdose ist leer."
  let frequencyList = [('e', 5), ('s', 3), (' ', 3), ('d', 2), ('i', 2), ('k', 2), ('o', 1), ('t', 1), ('l', 1), ('r', 1), ('.', 1)]
  assertEqual "count" (Map.fromList frequencyList) (countChars testString)
  assertEqual "insertFirst"  [0, 1, 3, 3, 7] (insertAscending id 0 [1, 3, 3, 7])
  assertEqual "insertMiddle" [1, 3, 3, 5, 7] (insertAscending id 5 [1, 3, 3, 7])
  assertEqual "insertLast"   [1, 3, 3, 7, 9] (insertAscending id 9 [1, 3, 3, 7])

  -- tree copied from the console output, but validated by hand.
  let expectedTree = Node {zeroPath = Node {zeroPath = Node {zeroPath = Node {zeroPath = Leaf {value = '.', frequency = 1}, onePath = Leaf {value = 'l', frequency = 1}, frequency = 2}, onePath = Node {zeroPath = Leaf {value = 'o', frequency = 1}, onePath = Leaf {value = 'r', frequency = 1}, frequency = 2}, frequency = 4}, onePath = Leaf {value = 'e', frequency = 5}, frequency = 9}, onePath = Node {zeroPath = Node {zeroPath = Leaf {value = ' ', frequency = 3}, onePath = Leaf {value = 's', frequency = 3}, frequency = 6}, onePath = Node {zeroPath = Node {zeroPath = Leaf {value = 't', frequency = 1}, onePath = Leaf {value = 'd', frequency = 2}, frequency = 3}, onePath = Node {zeroPath = Leaf {value = 'i', frequency = 2}, onePath = Leaf {value = 'k', frequency = 2}, frequency = 4}, frequency = 7}, frequency = 13}, frequency = 22}
  let actualTree = buildHTree testString
  assertEqual "consistent" True (isConsistent actualTree)
  assertEqual "build" expectedTree actualTree
  pure ()

test_toDecodeTree :: IO ()
test_toDecodeTree = do
  assertEqual "tree" expectedZeroFrequencyTree (toDecodeTree expectedCodingTable)
  pure ()

do_toDecodeTree_toCodingTable :: String -> IO ()
do_toDecodeTree_toCodingTable string = do
  let tree = buildHTree string
  assertEqual string (eraseFrequency tree) (toDecodeTree $ toCodingTable tree)
  pure ()

test_toDecodeTree_toCodingTable :: IO ()
test_toDecodeTree_toCodingTable = do
  let test = do_toDecodeTree_toCodingTable
  assertEqual "zeroed" expectedZeroFrequencyTree (eraseFrequency automaticSampleTree)
  test "Keks"
  test "wubwubwub"
  test "test_toDecodeTree_toCodingTable"
  pure ()

test_toFromWord8 :: IO ()
test_toFromWord8 = do
  reportNotImplemented "toFromWord8"
  pure ()

do_encodeDecode :: String -> IO ()
do_encodeDecode initial = do
  let transformed = compressHuffman initial
  let result = decompressHuffman transformed
  assertEqual "round trip" initial result
  assertEqual "transformed" False (initial == transformed)

test_encodeDecode :: IO ()
test_encodeDecode = do
  do_encodeDecode "wubwubwub"
  do_encodeDecode "do_encodeDecode"
  do_encodeDecode "this is another string"
  do_encodeDecode "thinking about it, this is actually the ideal case for property testing. anyways, back to work!"
  pure ()

test_encodeDecodeFile :: IO ()
test_encodeDecodeFile = do 
  reportNotImplemented "encodeDecodeFile"
  pure ()

allTests :: Test
allTests =
  TestList
    [ mkTest "toCodingTable" test_toCodingTable
    , mkTest "encode" test_encode
    , mkTest "isConsistent" test_isConsistent
    , mkTest "decode" test_decode
    , mkTest "buildHTree" test_buildHTree
    , mkTest "toDecodeTree" test_toDecodeTree
    , mkTest "toDecodeTree_toCodingTable" test_toDecodeTree_toCodingTable
    , mkTest "toFromWord8" test_toFromWord8
    , mkTest "encodeDecode" test_encodeDecode
    , mkTest "encodeDecodeFile" test_encodeDecodeFile
    ]
  where
    mkTest label ass = TestLabel label (TestCase ass)

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then exitWith ExitSuccess
    else exitWith (ExitFailure 1)
