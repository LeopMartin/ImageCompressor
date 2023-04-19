--
-- EPITECH PROJECT, 2022
-- B-FUN-400-MAR-4-1-compressor-leop.martin
-- File description:
-- Main.hs
--

module Main (main) where

-- import System.Directory
import System.Environment
import System.Exit

-----------------------------------GEST ARGS-----------------------------------

data Arguments = Arguments {
    numColors :: Int,
    convergenceLimit :: Double,
    filePath :: FilePath
}

parseArgs :: [String] -> Maybe Arguments
parseArgs ["-n", n, "-l", l, "-f", f] =
    Just $ Arguments {
        numColors = read n,
        convergenceLimit = read l,
        filePath = f
    }
parseArgs _ = Nothing

parseLine :: String -> (Int, Int, Int, Int, Int)
parseLine line =
    let nums = map read $ words [if c `elem` ",()" then ' ' else c | c <- line]
    in case nums of
        [a,b,c,d,e] -> (a,b,c,d,e)
        _ -> error "Invalide input"

isNegI :: Int -> IO ()
isNegI x | x <= 0 = exitWith (ExitFailure 84)
         | otherwise = return ()

isNegD :: Double -> IO ()
isNegD x | x <= 0 = exitWith (ExitFailure 84)
         | otherwise = return ()

-- isValidFilePath :: FilePath -> String
-- isValidFilePath filePath = do
--     if ((doesFileExist filePath) == False)
--         then exitWith (ExitFailure 84)
--     else return (readFile filePath)

----------------------------------INIT-----------------------------------------

tupleListSize :: [(a, b, c, d, e)] -> Int
tupleListSize [] = 0
tupleListSize (_:xs) = 1 + tupleListSize xs

tupleIsIn :: (Int, Int, Int, Int, Int) -> [(Int, Int, Int, Int, Int)] -> Bool
tupleIsIn a [] = False
tupleIsIn a (w:ws) | (tupleIsEq a w) == True = True
                   | otherwise = (tupleIsIn a ws)

tupleIsEq :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Bool
tupleIsEq (a, b, c, d, e) (f, g, k, l, m) | c == k && d == l && e == m = True
                                          | otherwise = False

initKmeans :: Int -> [(Int, Int, Int, Int, Int) ]->
    [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)]
initKmeans k [] c = c
initKmeans k (w:ws) c = if ((length c) == k)
                            then c
                        else if ((tupleIsIn w c) == False)
                                then let newval = c ++ [w]
                                    in initKmeans k ws newval
                             else initKmeans k ws c

---------------------------------RESOLUTION------------------------------------

--               the rands pixs       all pixel              list int
kmeansAlgo0 :: [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> [Int] -> [Int]
kmeansAlgo0 a [] f = f
kmeansAlgo0 a (x:xs) f = do
        let new_list = f ++ [(kmT x a 0 0 0)]
        kmeansAlgo0 a xs new_list

--                  allPix            rand pix                   ResultHigh | Nindexactuelle | Nindex list rand pix
kmT :: (Int, Int, Int, Int, Int) -> [(Int, Int, Int, Int, Int)] -> Double -> Int -> Int -> Int
kmT a [] b n t = n
kmT a (w:ws) 0 n t = kmT a ws (kmeansCalc (tupleToDouble a) (tupleToDouble w)) n (t + 1)
kmT a (w:ws) b n t | b > (kmeansCalc (tupleToDouble a) (tupleToDouble w)) = kmT a ws (kmeansCalc (tupleToDouble a) (tupleToDouble w)) t (t + 1)
                   | b <= (kmeansCalc (tupleToDouble a) (tupleToDouble w)) = kmT a ws b n (t + 1)

toInt :: Double -> Int
toInt x = round x

tupleToDouble :: (Int, Int, Int, Int, Int) -> (Double, Double, Double, Double, Double)
tupleToDouble (a, b, c, d, e) = (toDouble a, toDouble b, toDouble c, toDouble d, toDouble e)
  where
    toDouble :: (Real a) => a -> Double
    toDouble = realToFrac

kmeansCalc :: (Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double) -> Double
kmeansCalc (x1, y1, r1, g1, b1) (x2, y2, r2, g2, b2) = sqrt $ sum [(r1 - r2) ^ 2, (g1 - g2) ^ 2, (b1 - b2) ^ 2]

-- kmeans : verifier si lequel des point et le proche de nos x cluster (par couleur)

---------------------------------MOYEN CERCLOID------------------------------

            --   limit    inc     index              allPix                   result
mergeCercloid :: Int -> Int -> [Int] -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)]
mergeCercloid e i l a b = if (i /= e)
                            then let new_val = b ++ [(mergerE i l a [])]
                            in (mergeCercloid e (i + 1) l a new_val)
                        else b

        -- inc    index              allPix                   list point n                     new cercloid
mergerE :: Int -> [Int] -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> (Int, Int, Int, Int, Int)
mergerE n a [] c = (average c)
mergerE n [] b c = (average c)
mergerE n (a:ax) (b:bx) c | n == a = do
                                let new_list = c ++ [b]
                                (mergerE n ax bx new_list)
                            | otherwise = (mergerE n ax bx c)


average :: [(Int, Int, Int, Int, Int)] -> (Int, Int, Int, Int, Int)
average [] = error "Empty list"
average xs =
  let (s1, s2, s3, s4, s5) = foldr (\(a, b, c, d, e) (acc1, acc2, acc3, acc4, acc5) ->
                                      (a + acc1, b + acc2, c + acc3, d + acc4, e + acc5))
                                   (0, 0, 0, 0, 0) xs
      n = length xs
  in (0, 0, s3 `div` n, s4 `div` n, s5 `div` n)


---------------------------------AFFICHAGE DE FIN------------------------------

--      inc   limit  list_nb            all_pix                          rand_pix
pRBE :: Int -> Int -> [Int] -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> IO ()
pRBE i l n p [] = putStr ""
pRBE i l n p (r:rx) = do
    printCluster r
    pAPix i p n
    pRBE (i + 1) l n p rx

--      group              all_pix             list_ap
pAPix :: Int -> [(Int, Int, Int, Int, Int)] -> [Int] -> IO ()
pAPix g [] b = putStr ""
pAPix g a [] = putStr ""
pAPix g (a:ax) (b:bx) | g == b = do
                            printPixel (a)
                            (pAPix g ax bx)
                      | otherwise = (pAPix g ax bx)

printCluster :: (Int, Int, Int, Int, Int) -> IO ()
printCluster (a, b , c, d, e) = do
    putStrLn "--"
    putStrLn $ "(" ++ show c ++ "," ++ show d ++ "," ++ show e ++ ")"
    putStrLn "-"

-- (0,1) (98,99,233)
printPixel :: (Int, Int, Int, Int, Int) -> IO ()
printPixel (a, b , c, d, e) = putStrLn $ "(" ++ show a ++ "," ++ show b ++ ") " ++ "(" ++ show c ++ "," ++ show d ++ "," ++ show e ++ ")"

---------------------------------MAIN------------------------------------------

minusConv :: Double -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> Bool -- revois True si c'est bon et False si c'est mauvais
minusConv n [] b = True
minusConv n a [] = True
minusConv n (a:ax) (b:bx) | ((toInt(kmeansCalc (tupleToDouble a) (tupleToDouble b))) * 10) < ((toInt n) * 10) = False
                          | otherwise = minusConv n ax bx
-- une fonction qui prend une cluster

--             n_cerc   conv               allpix                   rand_cercloid t0                  rand t1                list_n
boucleFinal :: Int -> Double -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)] -> [Int] -> IO ()
boucleFinal n c a b [] l = do
        let listPos = (kmeansAlgo0 b a [])
        let cercloid = (mergeCercloid n 0 listPos a [])
        (boucleFinal n c a cercloid b listPos)

boucleFinal n c a b x l = if ((minusConv c b x) == False) -- faire un truc qui check tt les cecloid son bien en dessous du seuil
                            then let listPos = (kmeansAlgo0 b a [])
                            in let cercloid = (mergeCercloid n 0 listPos a [])
                            in (boucleFinal n c a cercloid x listPos)
                          else
                            pRBE 0 n (kmeansAlgo0 b a []) a (mergeCercloid n 0 (kmeansAlgo0 b a []) a [])

---------------------------------MAIN------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> exitWith (ExitFailure 84)
        Just (Arguments n l f) -> do
            fileResult <- readFile f
            isNegI n
            isNegD l
            let linesOfFile = lines fileResult
            let allPix = map parseLine linesOfFile
            let randPix = (initKmeans n allPix [])
            let listPos = (kmeansAlgo0 randPix allPix [])
            let cercloid = (mergeCercloid n 0 listPos allPix [])
            boucleFinal n l allPix cercloid [] listPos
