import System.Random ( mkStdGen, Random(randomRs) )
import Data.Time ( diffUTCTime, getCurrentTime )
import Text.Printf ( printf )

genRandomList :: Int -> Int -> Int -> [Int]
genRandomList count minValue maxValue =
    take count $ randomRs (minValue, maxValue) (mkStdGen 42)

maxProfitWithDays :: [Int] -> (Int, Int, Int, Int)
maxProfitWithDays arr = fst $ foldl processDay ((0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int), (head arr, 1 :: Int)) $ zip [1..] $ tail $ scanl (+) 0 arr
    where
        processDay ((buyPrice, sellPrice, buyDay, sellDay), (minTemp, indexMinTemp)) (day, price)
          | price - minTemp > sellPrice - buyPrice =
            let newBuyPrice = minTemp
                newSellPrice = price
                newBuyDay = indexMinTemp
                newSellDay = day
            in ((newBuyPrice, newSellPrice, newBuyDay, newSellDay), (minTemp, indexMinTemp))
          | price < minTemp =
            let newMinTemp = price
                newIndexMinTemp = day
            in ((buyPrice, sellPrice, buyDay, sellDay), (newMinTemp, newIndexMinTemp))
          | otherwise =
            ((buyPrice, sellPrice, buyDay, sellDay), (minTemp, indexMinTemp))

main :: IO ()
main = do
    let arr = genRandomList 100000000 (-1000) 1000
    start <- getCurrentTime
    let (buyVal, sellVal, buyDay, sellDay) = maxProfitWithDays arr
    printf "Buy: %d, Sell: %d, BuyDay: %d, SellDay: %d, Profit: %d\n" buyVal sellVal buyDay sellDay (sellVal - buyVal)
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) * 1000 :: Double
    printf "Time: %.6f ms\n" diff
