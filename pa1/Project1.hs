module Project1 where 

div7or9 :: Integer -> Bool
div7or9 x =
    let d7 = div x 7
        d9 = div x 9
        r7 = 7 * d7
        r9 = 9 * d9
    in case (r7 == x) || (r9 == x) of 
        True ->  True
	False -> False
        

echo :: [Char] -> [Char]
echo x = case length x of
	0 -> []
	_ -> [head x] ++ [head x] ++ echo (tail x)
    

echons :: [Char] -> [Char]
echons x =  case ((length x),x) of
	(0,_) -> []
	(_ , n) | head n == ' ' -> [head x] ++ echons (tail x) 
	(_ , n) | head n /= ' '-> [head x] ++ [head x] ++ echons (tail x)

countEvens :: [Integer] -> Integer
countEvens x = case (length x,x) of
	(0,_) -> 0
	(_, n ) | even (head n)  == True -> 1+(countEvens (tail x))
	(_,_) -> countEvens (tail x)



getfirstelem :: [(Double,Double)]->[Double]
getfirstelem l = [x | (x,y) <- l]

getsecondelem :: [(Double,Double)]->[Double]
getsecondelem l = [y | (x,y) <- l]

average :: [Double] -> Double
average l = sum l / fromIntegral (length l)


centroid :: [(Double,Double)] -> (Double,Double)
centroid x = (average (getfirstelem x), average (getsecondelem x)) 

generateSequence :: [Integer] -> [Integer]
generateSequence x = case last x of
	1 -> x
	a |even a -> generateSequence (x ++ [div (last x) 2])
	a |odd a -> generateSequence  (x ++ [3 * (last x) + 1])


hailstone :: Integer -> Integer
hailstone x = fromIntegral (length (generateSequence [x]))
