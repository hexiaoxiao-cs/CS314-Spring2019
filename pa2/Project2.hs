module Project2 where

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


sadd :: Sparse -> Sparse -> Sparse
sadd = undefined
