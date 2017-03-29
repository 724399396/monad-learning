-- allCombinations returns a list containing the rsult of
-- folding the binary operator through all combinations
-- of elements of the giben lists
-- For example, allCombinations (+) ([0,1], [1,2,3]] would be
--  [0+1, 0+2, 0+3, 1+1, 1+2, 1+3] or [1,2,3,2,3,4]
-- and allComnations (*) [[0,1],[1,2],[3,5]] would be
--  [0*1*3,0*1*5,0*2*3,0*2*5,1*1*3,1*1*5,1*2*3,1*2*5], or [0,0,0,0,3,5,6,10]
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombiantions fn [] = []
allCombinations fn (l:ls) = foldl (liftM2 fn) l ls
