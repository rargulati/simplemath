{-
take some input, clean it, transform to appropriate type
read parses strings and produces values
which we then take and transform to the value type
that makes sense in this context
-}
clean raw = map (\s -> read s :: Double) (lines raw)

{-
a (alpha) is a user-defined constant
in moving avg, higher a (closer to 1) gives more weight
to newer data (discounting past avg as seen by 1-a)
lower alpha gives higher weight to past avg
-}
avg :: [Double] -> Double
avg (x:xs) = (a * x) + ((1 - a) * (avg xs))
  where a = 0.95
avg [] = 0.0

{-
standard mean calculation
fromIntegral converts from any Int to Num
necessary as the sum function gives us a Num type
Sum: monoid under addition
-}
mean xs = (sum xs) / (fromIntegral (length xs))

-- find the difference between the two types of calculating averages
splitError :: [Double] -> Double
splitError xs = mean xs - avg xs

-- main loop that runs the above functions
main = do
  streamData <- readFile "input.txt"
  let input = clean streamData
  putStrLn $ "the mean is: " ++ (show. mean) input
  putStrLn $ "the moving average is: " ++ (show. avg) input
  putStrLn $ "the difference between the two is: " ++ (show . splitError) input
