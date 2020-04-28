import Lib
import Util
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lib.solve" $ do
    it "can solve an easy sudoku" $ do
      solve testBoard `shouldBe` 
        [ [5,3,4,6,7,8,9,1,2]
        , [6,7,2,1,9,5,3,4,8]
        , [1,9,8,3,4,2,5,6,7]
        , [8,5,9,7,6,1,4,2,3]
        , [4,2,6,8,5,3,7,9,1]
        , [7,1,3,9,2,4,8,5,6]
        , [9,6,1,5,3,7,2,8,4]
        , [2,8,7,4,1,9,6,3,5]
        , [3,4,5,2,8,6,1,7,9]
        ]
    xit "can solve harder board" $ do
      solve hardBoard `shouldBe`
        [ [1,7,2,5,4,9,6,8,3]
        , [6,4,5,8,7,3,2,1,9]
        , [3,8,9,2,6,1,7,4,5]
        , [4,9,6,3,2,7,8,5,1]
        , [8,1,3,4,5,6,9,7,2]
        , [2,5,7,1,9,8,4,3,6]
        , [9,6,4,7,1,5,3,2,8]
        , [7,3,1,6,8,2,5,9,4]
        , [5,2,8,9,3,4,1,6,7]
        ]


testBoard :: WorkingBoard
testBoard = 
  convert 
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
  , [6, 0, 0, 1, 9, 5, 0, 0, 0]
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

hardBoard :: WorkingBoard
hardBoard = 
  convert 
  [ [0, 0, 0, 0, 0, 0, 6, 8, 0]
  , [0, 0, 0, 0, 7, 3, 0, 0, 9]
  , [3, 0, 9, 0, 0, 0, 0, 4, 5]
  , [4, 9, 0, 0, 0, 0, 0, 0, 0]
  , [8, 0, 3, 0, 5, 0, 9, 0, 2]
  , [0, 0, 0, 0, 0, 0, 0, 3, 6]
  , [9, 6, 0, 0, 0, 0, 3, 0, 8]
  , [7, 0, 0, 6, 8, 0, 0, 0, 0]
  , [0, 2, 8, 0, 0, 0, 0, 0, 0]
  ]

convert :: [[Int]] -> WorkingBoard
convert board = parseBoard $ map2D convertTile board
  where
    convertTile n
      | n >= 1 && n <= 9 = NonEmpty n
      | otherwise = Empty
