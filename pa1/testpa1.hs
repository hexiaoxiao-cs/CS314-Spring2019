module Main where

import Control.Exception
import Data.List
import System.Timeout
import Test.QuickCheck
import qualified Project1 as P

-- Reference implementations

div7or9 :: Integer -> Bool
div7or9 n = n `mod` 7 == 0 || n `mod` 9 == 0

echo :: [Char] -> [Char]
echo [] = []
echo (x:xs) = x:x:echo xs

echons :: [Char] -> [Char]
echons [] = []
echons (' ':xs) = ' ':echons xs
echons (x:xs) = x:x:echons xs

countEvens :: [Integer] -> Integer
countEvens = fromIntegral . length . filter even

centroid :: [(Double,Double)] -> (Double,Double)
centroid points = (sumx / count, sumy / count)
    where
    count = fromIntegral (length points)
    sumx = sum (map fst points)
    sumy = sum (map snd points)


hailstone :: Integer -> Integer
hailstone = fromIntegral . length . hailstones

hailstones :: Integer -> [Integer]
hailstones 1 = [1]
hailstones n = n : hailstones (if even n then n `div` 2 else n * 3 + 1)


-- Properties

prop_div7or9 :: Integer -> Bool
prop_div7or9 i = P.div7or9 i == div7or9 i

prop_echo_length :: String -> Bool
prop_echo_length s = length (P.echo s) == 2 * length s

prop_echo :: String -> Bool
prop_echo s = P.echo s == echo s

prop_echons_length :: String -> Bool
prop_echons_length s = length (P.echons s) >= length s

prop_echons :: String -> Bool
prop_echons s = P.echons s == echons s

prop_countEvens_nonneg :: [NonNegative Integer] -> Bool
prop_countEvens_nonneg = prop_countEvens . map getNonNegative

prop_countEvens :: [Integer] -> Bool
prop_countEvens l = P.countEvens l == countEvens l

distance (a,b) (x,y) = sqrt ((a-x)^2 + (b-y)^2)

prop_centroid :: NonEmptyList (Double,Double) -> Bool
prop_centroid (NonEmpty pts) = distance (P.centroid pts) (centroid pts) < 1e-5

prop_hailstone :: Positive Integer -> Bool
prop_hailstone (Positive i) = P.hailstone i == hailstone i


-- Test cases

timelimit = 1000000

testBy :: (Show a) => (a -> a -> Bool) -> String -> a -> a -> IO Bool
testBy eq name got want = catch body (\(ErrorCall s) -> fail "ERROR" s)
    where
    body = do
        ok <- timeout timelimit (evaluate (eq got want))
        case ok of
            Just True -> return True
            Just False -> fail "FAIL " (show got)
            Nothing -> fail "TIMEOUT" "impatient"

    fail msg txt = do
        putStrLn $ msg ++ ": " ++ name
        putStrLn $ "       wanted: " ++ show want
        putStrLn $ "       got:    " ++ txt
        return False

test :: (Eq a, Show a) => String -> a -> a -> IO Bool
test = testBy (==)


tests =
    [ ("div7or9",
        [ test "div7or9 0" (P.div7or9 0) True
        , test "div7or9 1" (P.div7or9 1) False
        , test "div7or9 7" (P.div7or9 7) True
        , test "div7or9 9 "(P.div7or9 9) True
        ],
        [ property prop_div7or9
        ])
    , ("echo",
        [ test "echo \"abc\"" (P.echo "abc") "aabbcc"
        , test "echo \"d6 ?\"" (P.echo "d6 ?") "dd66  ??"
        , test "echo \" . a f\"" (P.echo " . a f") "  ..  aa  ff"
        , test "echo \"   \"" (P.echo "   ") "      "
        ],
        [ property prop_echo_length
        , property prop_echo
        ])
    , ("echons",
        [ test "echons \"abc\"" (P.echons "abc") "aabbcc"
        , test "echons \"d6 ?\"" (P.echons "d6 ?") "dd66 ??"
        , test "echons \" . a f\"" (P.echons " . a f") " .. aa ff"
        , test "echons \"   \"" (P.echons "   ") "   "
        ],
        [ property prop_echons
        ])
    , ("countEvens",
        [ test "countEvens [0,1,2,3]" (P.countEvens [0,1,2,3]) 2
        , test "countEvens [3]" (P.countEvens [3]) 0
        , test "countEvens []" (P.countEvens []) 0
        , test "countEvens [4,6,7,8]" (P.countEvens [4,6,7,8]) 3
        ],
        [ property prop_countEvens_nonneg
        , property prop_countEvens
        ])
    , ("centroid",
        [ test "centroid [(0,0)]" (P.centroid [(0,0)]) (0,0)
        , test "centroid [(0,0),(2,2)]" (P.centroid [(0,0),(2,2)]) (1,1)
        , test "centroid [(0,0),(0,2),(4,4)]" (P.centroid [(2,0),(0,2),(4,4)]) (2,2)
        , test "centroid [(10,1),(8,5),(-3,2),(2,2),(-1,1),(2,1)]" (P.centroid [(10,1),(8,5),(-3,2),(2,2),(-1,1),(2,1)]) (3,2)
        ],
        [ property prop_centroid
        ])
    , ("hailstone",
        [ test "hailstone 3" (P.hailstone 3) 8
        , test "hailstone 1" (P.hailstone 1) 1
        , test "hailstone 8" (P.hailstone 8) 4
        , test "hailstone 104" (P.hailstone 104) 13
        , test "hailstone 103" (P.hailstone 103) 88
        ],
        [ property prop_hailstone
        ])
    ]

args = stdArgs { maxSuccess = 500 }

runprop prop = do
    r <- quickCheckWithResult args (within timelimit prop)
    case r of
        Success{} -> return True
        _ -> return False


runTestGroup (name, units, props) = do
    putStrLn $ "\nTesting " ++ name
    unitpass <- fmap (length . filter id) (sequence units)
    putStrLn $ "Unit tests passed: " ++ show unitpass
    proppass <- fmap (genericLength . filter id) (mapM runprop props)
    return $ 5.0 * (fromIntegral unitpass / genericLength units +
                    proppass / genericLength props)


runTests groups = do
    scores <- mapM runTestGroup groups
    putStrLn $ "Score: " ++ show (sum scores)


main = runTests tests
