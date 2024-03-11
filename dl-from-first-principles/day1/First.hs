gradientDescent :: Double -> Double -> Double -> Double
gradientDescent alpha x0 epsilon = go x0
  where
    -- The derivative of our function
    df x = 2 * (x - 3)

    -- Recursive helper function that performs the iteration
    go x
      | abs (df x) < epsilon = x -- Stop when the change is small
      | otherwise = go (x - alpha * df x) -- Update x and recurse

main :: IO ()
main = do
  let alpha = 0.01 -- Learning rate
      initialX = 0.0 -- Initial guess
      epsilon = 0.0001 -- Tolerance for stopping
      minimumX = gradientDescent alpha initialX epsilon
  putStrLn $ "Minimum found at x = " ++ show minimumX
