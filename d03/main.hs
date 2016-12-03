-- sides lengths of a triangle
type Triangle = (Int, Int, Int)

-- checks if a triangle is possible
isValid :: Triangle -> Bool
isValid (a, b, c) = a + b > c && a + c > b && b + c > a

-- parses a string with 3 integers as a triangle
parseTriangle :: String -> Triangle
parseTriangle s = case words s of
    [a, b, c] -> (read a, read b, read c)
    _         -> (0, 0, 0)

-- parses a list of rows with integers as a list of triangles
parseTriangles2 :: [String] -> [Triangle]
parseTriangles2 (r1:r2:r3:rs) = let
        ir1 = map read $ words r1
        ir2 = map read $ words r2
        ir3 = map read $ words r3
    in
        zip3 ir1 ir2 ir3 ++ parseTriangles2 rs
parseTriangles2 _ = []

main :: IO ()
main = do
    contents <- readFile "input.txt"

    let triangles1 = map parseTriangle $ lines contents
    let triangles2 = parseTriangles2 $ lines contents

    print . length . filter isValid $ triangles1
    print . length . filter isValid $ triangles2
