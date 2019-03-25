module Project2 where
import Data.List (sort)
--import Debug.Trace

zero :: Int -> Int -> [[Double]]
zero rows cols = take rows (repeat (take cols (repeat 0.0)))

oneinplace :: Int -> Int -> [Double]
oneinplace n r = take (r) (repeat 0.0) ++ [1] ++ take (n-r-1) (repeat 0.0) 

ident :: Int -> [[Double]]
ident n = map (oneinplace n) [0..n-1]

getNth :: [[Double]] -> Int -> [Double]
getNth matrix n = if n == 0
                  then []
                  else getNth matrix (n-1) ++ [((matrix !! (n-1)) !! (n-1))]

diag :: [[Double]] -> [Double]
diag matrix = getNth matrix (min (length matrix) (length (head matrix)))

add :: [[Double]] -> [[Double]] -> [[Double]]
add matrix1 matrix2 = zipWith (zipWith (+)) matrix1 matrix2

transp :: [[Double]] -> [[Double]]
transp ([]:_) = []
transp matrix = (map head matrix) : (transp (map tail matrix))


--

data Sparse = Sparse Int Int [(Int,[(Int,Double)])]
    deriving (Show, Eq)

sident :: Int -> Sparse
sident n = Sparse n n (map (\x -> (x,[(x,1.0)])) [0..n-1])

getdiag :: Int -> [(Int,Double)] -> Double
getdiag x z | (null z) = 0.0
            | (fst (head z)) == x = snd (head z)
            | otherwise = getdiag x (tail z)

getdiag2 :: Int -> [(Int, [(Int,Double)])] -> [(Int,Double)]
getdiag2 x z | (null z) = []
             | (fst (head z)) == x = snd (head z)
             | otherwise = getdiag2 x (tail z)


sdiag :: Sparse -> [Double]
sdiag (Sparse x y z) = map (\k -> getdiag k (getdiag2 k z)) [0..(min x y)-1]

lineadd :: Int ->[(Int,Double)] ->[(Int,Double)] -> [(Int,Double)]
--lineadd counts l1 l2 | trace ("lineadd "++ show l1 ++ " " ++ show l2) False = undefined
lineadd counts l1 l2 | (null l1) = l2
                     | (null l2) = l1
                     | (fst (head l1)) == (fst (head l2)) = [(fst (head l1), (snd (head l1) + snd (head l2)))] ++ lineadd counts (tail l1) (tail l2)
                     | (fst (head l1)) > (fst (head l2)) = [(head l2)] ++ lineadd counts l1 (tail l2)
                     | (fst (head l1)) < (fst (head l2)) = [(head l1)] ++ lineadd counts (tail l1) l2

shrinkline :: [(Int,Double)] -> [(Int,Double)]
shrinkline a | null a = []
             | (snd (head a))==0 = shrinkline (tail a)
             | otherwise = [(head a)] ++ shrinkline (tail a)

shrinkwhole :: [(Int,[(Int,Double)])] -> [(Int,[(Int,Double)])]
shrinkwhole a | null a = []
              | null (snd (head a)) = shrinkwhole (tail a)
              | otherwise = [(head a)] ++ shrinkwhole (tail a)



sadd :: Sparse -> Sparse -> Sparse
sadd (Sparse a b c) (Sparse d e f) = Sparse (max a d) (max b e) (shrinkwhole (map (\k -> (k,(shrinkline(lineadd (max b e) (sort (getdiag2 k c)) (sort (getdiag2 k f)))))) [0..(max a d)-1]))