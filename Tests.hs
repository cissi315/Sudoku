

{- HUFFMAN EXEMPEL PÅ TESTCASES
-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

addedtest1 = TestCase $ assertEqual "huffmanTree empty"
                Void (huffmanTree $ characterCounts "")
addedtest2 = TestCase $ assertEqual "huffmanTree single argument"
                (Leaf 't' 1) (huffmanTree $ characterCounts "t")
addedtest3 = TestCase $ assertEqual "This is a huffmanTree"
              testHuffmanTree (huffmanTree $ characterCounts "this is an example of a huffman tree")

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, addedtest1, addedtest2, addedtest3] -}