-- take some input, clean it, cast to appropriate type
clean raw = map (\s -> read s :: Double) (lines raw)

-- a (alpha) is a user-defined constant
-- in moving avg, higher a (closer to 1) gives more weight
-- to newer data (discounting past avg as seen by 1-a)
-- lower alpha gives higher weight to past avg
avg :: [Double] -> Double
avg (x:xs) = (a * x) + ((1 - a) * (avg xs))
  where a = 0.95
avg [] = 0.0

-- standard mean calculation
-- fromIntegral converts from any Int to Num
-- necessary as the sum function gives us a Num type
-- Sum: monoid under addition
let mean xs = sum xs / (fromIntegral (length xs))
