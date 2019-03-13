module Project2 where

zero :: Int -> Int -> [[Double]]
zero = undefined

ident :: Int -> [[Double]]
ident = undefined


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
