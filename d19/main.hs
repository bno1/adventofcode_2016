puzzleInput :: Int
puzzleInput = 3014387

-- solves part 1 in O(log(n))
-- after a full circle there remain n / 2 elves (integer division)
-- so we call the function again for n / 2 elves and take the returned elve index
-- and map it back to our initial n elves:
-- solution = 2 * elve_number - 1 for even n or + 1 for odd n
--
-- could be done in O(1) but I'm too lazy to deduce the formula
-- see https://en.wikipedia.org/wiki/Josephus_problem
solve1 :: Int -> Int
solve1 n
    | n == 1 = 1
    | even n = 2 * solve1 (n `quot` 2) - 1
    | odd  n = 2 * solve1 (n `quot` 2) + 1
    | otherwise = error $ show n

-- solves part 2 in O(n)
-- the removed elve is n / 2 (integer division)
-- then calls itself again for (n - 1) elves and map the returned value back to our
-- original n elves:
-- elve_number if it's before the removed elve or elve_number + 1 mod n if it's
-- >= removed elve (because the removed elve is ignore by solve2 (n - 1))
-- I think it could be done faster than O(n) but it's tricky
solve2 :: Int -> Int
solve2 1 = 1
solve2 n = 1 + if r >= removed then (r + 1) `mod` n else r
    where removed = n `quot` 2
          r = solve2 (n - 1)

main :: IO ()
main = do
    print $ solve1 puzzleInput
    print $ solve2 puzzleInput
