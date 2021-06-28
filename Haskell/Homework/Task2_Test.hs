module Task2_Test where

import Test.HUnit
import Task2

zeroExampleImage :: Image
zeroExampleImage = Image 0 0 []

firstExampleImage :: Image        --(0,0)       --(0,1)         --(0,2)
firstExampleImage = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],
                                  --(1,0)       --(1,1)         --(1,2)
                               [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]


firstExampleGrayscale = Image {width = 3, height = 2, content = [[Rgb {red = 76, green = 76, blue = 76}
                                                                   ,Rgb {red = 122, green = 122, blue = 122}
                                                                   ,Rgb {red = 227, green = 227, blue = 227}]
                                                                 ,[Rgb {red = 150, green = 150, blue = 150}
                                                                   ,Rgb {red = 255, green = 255, blue = 255}
                                                                   ,Rgb {red = 203, green = 203, blue = 203}]]}

firstExampleFloodFill = Image {width = 3, height = 2, content = [[Rgb {red = 255, green = 0, blue = 0}
                                                                   ,Rgb {red = 42, green = 42, blue = 42}
                                                                   ,Rgb {red = 255, green = 255, blue = 0}]
                                                                 ,[Rgb {red = 0, green = 255, blue = 0}
                                                                   ,Rgb {red = 255, green = 255, blue = 255}
                                                                   ,Rgb {red = 128, green = 255, blue = 128}]]}


secondExampleImage :: Image       --(0,0)       --(0,1)         --(0,2)
secondExampleImage = Image 3 2 [[Rgb 255 0 0, Rgb 255 255 0, Rgb 255 255 0],
                                  --(1,0)       --(1,1)         --(1,2)
                                [Rgb 0 255 0, Rgb 255 255 0, Rgb 255 255 0]]

secondExampleGrayscale = Image {width = 3, height = 2, content = [[Rgb {red = 76, green = 76, blue = 76}
                                                                    ,Rgb {red = 227, green = 227, blue = 227}
                                                                    ,Rgb {red = 227, green = 227, blue = 227}]
                                                                  ,[Rgb {red = 150, green = 150, blue = 150}
                                                                    ,Rgb {red = 227, green = 227, blue = 227}
                                                                    ,Rgb {red = 227, green = 227, blue = 227}]]}

secondExampleFloodFill = Image {width = 3, height = 2, content = [[Rgb {red = 255, green = 0, blue = 0}
                                                                    ,Rgb {red = 55, green = 55, blue = 55}
                                                                    ,Rgb {red = 55, green = 55, blue = 55}]
                                                                  ,[Rgb {red = 0, green = 255, blue = 0}
                                                                    ,Rgb {red = 55, green = 55, blue = 55}
                                                                    ,Rgb {red = 55, green = 55, blue = 55}]]}

thirdExampleImage :: Image       --(0,0)       --(0,1)         --(0,2)
thirdExampleImage = Image 3 3 [[Rgb 255 0 0, Rgb 0 255 0, Rgb 0 0 255],
                                 --(1,0)       --(1,1)         --(1,2)
                               [Rgb 0 255 0, Rgb 0 255 0, Rgb 0 255 0],
                                 --(2,0)       --(2,1)         --(2,2)
                               [Rgb 255 0 0, Rgb 0 255 0, Rgb 0 0 255]]

thirdExampleFloodFill = Image {width = 3, height = 3, content = [[Rgb {red = 255, green = 0, blue = 0}
                                                                   ,Rgb {red = 123, green = 123, blue = 123}
                                                                   ,Rgb {red = 0, green = 0, blue = 255}]
                                                                 ,[Rgb {red = 123, green = 123, blue = 123}
                                                                   ,Rgb {red = 123, green = 123, blue = 123}
                                                                   ,Rgb {red = 123, green = 123, blue = 123}]
                                                                 ,[Rgb {red = 255, green = 0, blue = 0}
                                                                   ,Rgb {red = 123, green = 123, blue = 123}
                                                                   ,Rgb {red = 0, green = 0, blue = 255}]]}

-- grayscale
testZeroExampleGrayscale = TestCase (assertEqual "grayscale should work correctly for the empty image" zeroExampleImage (grayscale zeroExampleImage))
testFirstExampleGrayscale = TestCase (assertEqual "grayscale should work correctly for the first example" firstExampleGrayscale (grayscale firstExampleImage))
testSecondExampleGrayscale = TestCase (assertEqual "grayscale should work correctly for the second example" secondExampleGrayscale (grayscale secondExampleImage))

testsGrayscale = TestList [testZeroExampleGrayscale, testFirstExampleGrayscale, testSecondExampleGrayscale]

-- floodFill
testZeroExampleFloodFill = TestCase (assertEqual "floodFill should work correctly for the empty image" zeroExampleImage (floodFill (Rgb 0 0 0) 0 0 zeroExampleImage))
testFirstExampleFloodFill = TestCase (assertEqual "floodFill should work correctly for the first example" firstExampleFloodFill (floodFill (Rgb 42 42 42) 0 1 firstExampleImage))
testSecondExampleFloodFill = TestCase (assertEqual "floodFill should work correctly for the first example" secondExampleFloodFill (floodFill (Rgb 55 55 55) 1 2 secondExampleImage))
testThirdExampleFloodFill = TestCase (assertEqual "floodFill should work correctly for the first example" thirdExampleFloodFill (floodFill (Rgb 123 123 123) 1 1 thirdExampleImage))

testsFloodFill = TestList [testZeroExampleFloodFill, testFirstExampleFloodFill, testSecondExampleFloodFill, testThirdExampleFloodFill]

-- main
main = do
	runTestTT testsGrayscale
	runTestTT testsFloodFill