module Project2 where

zero :: Int -> Int -> [[Double]]
zero rows cols = take rows (repeat (take cols (repeat 0.0)))

oneinplace :: Int -> Int -> [Double]
oneinplace n r = take (r) (repeat 0) ++ [1] ++ take (n-r-1) (repeat 0) 

ident :: Int -> [[Double]]
ident n = map (oneinplace n) [0..n-1]


diag :: [[Double]] -> [Double]
diag = undefined


add :: [[Double]] -> [[Double]] -> [[Double]]
add = undefined


transp :: [[Double]] -> [[Double]]
transp = undefined


--

data Sparse = Sparse Int Int [(Int,[(Int,Double)])]
    deriving (Show, Eq)

sident :: Int -> Sparse
sident = undefined

sdiag :: Sparse -> [Double]
sdiag = undefined

sadd :: Sparse -> Sparse -> Sparse
sadd = undefined
