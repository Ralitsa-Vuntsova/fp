module Task1_Test where

import Test.HUnit
import Task1

zeroExample :: (Num a) => Tree a
zeroExample = EmptyTree

firstExampleTree :: (Num a) => Tree a
firstExampleTree = Node {value = 5
                   , left = Node {value = 22 
                                 , left = Node {value = 2, left =  EmptyTree, right = EmptyTree} 
                                 , right = Node {value = 6, left = EmptyTree, right = EmptyTree}}
                   , right = Node {value = 1
                                 , left = EmptyTree 
                                 , right = Node {value = 3
                                                , left = Node {value = 111, left = EmptyTree, right = EmptyTree} 
                                                , right = EmptyTree}}}

secondExampleTree :: (Num a) => Tree a
secondExampleTree = Node {value = 25
                    , left = Node {value = 15
                                  , left =  Node {value = 10
                                                 , left = Node {value = 4, left = EmptyTree, right = EmptyTree}
                                                 , right = Node {value = 12, left = EmptyTree, right = EmptyTree}}
                                  , right = Node {value = 22
                                                 , left = Node {value = 18, left = EmptyTree, right = EmptyTree}
                                                 , right = Node {value = 24, left = EmptyTree, right = EmptyTree}}} 
                    , right = Node {value = 50
                                  , left =  Node {value = 35
                                                 , left = Node {value = 31, left = EmptyTree, right = EmptyTree}
                                                 , right = Node {value = 44, left = EmptyTree, right = EmptyTree}}
                                  , right = Node {value = 70
                                                 , left = Node {value = 66, left = EmptyTree, right = EmptyTree}
                                                 , right = Node {value = 90, left = EmptyTree, right = EmptyTree}}}}

testEmptyTreePreorder = TestCase (assertEqual "Preorder should work correctly for the empty tree" [] (values Preorder zeroExample))
testEmptyTreeInorder = TestCase (assertEqual "Inorder should work correctly for the empty tree" [] (values Inorder zeroExample))
testEmptyTreePostorder = TestCase (assertEqual "Postorder should work correctly for the empty tree" [] (values Postorder zeroExample))

testFirstExamplePreorder = TestCase (assertEqual "Preorder should work correctly for the first example" [5,22,2,6,1,3,111] (values Preorder firstExampleTree))
testFirstExampleInorder = TestCase (assertEqual "Inorder should work correctly for the first example" [2,22,6,5,1,111,3] (values Inorder firstExampleTree))
testFirstExamplePostorder = TestCase (assertEqual "Postorder should work correctly for the first example" [2,6,22,111,3,1,5] (values Postorder firstExampleTree))

testSecondExamplePreorder = TestCase (assertEqual "Preorder should work correctly for the second example" [25,15,10,4,12,22,18,24,50,35,31,44,70,66,90] (values Preorder secondExampleTree))
testSecondExampleInorder = TestCase (assertEqual "Inorder should work correctly for the second example" [4,10,12,15,18,22,24,25,31,35,44,50,66,70,90] (values Inorder secondExampleTree))
testSecondExamplePostorder = TestCase (assertEqual "Postorder should work correctly for the second example" [4,12,10,18,24,22,15,31,44,35,66,90,70,50,25] (values Postorder secondExampleTree))

testsZeroExample = TestList [testEmptyTreePreorder, testEmptyTreeInorder, testEmptyTreePostorder]
testsFirstExample = TestList [testFirstExamplePreorder, testFirstExampleInorder, testFirstExamplePostorder]
testsSecondExample = TestList [testSecondExamplePreorder, testSecondExampleInorder, testSecondExamplePostorder]

main = do
	runTestTT testsZeroExample
	runTestTT testsFirstExample
	runTestTT testsSecondExample