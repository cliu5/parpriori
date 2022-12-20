import Control.Parallel.Strategies(parList, rseq, Eval, runEval, parListChunk)
import System.Environment(getArgs, getProgName)
import System.Exit(die)
import Data.Text as T
import Data.Map as M
import Data.Maybe(isNothing, fromJust)
import Data.List as L
import Data.Set as S

main :: IO ()
main = do 
      args <- getArgs
      case args of
         [filename, minsupS, minconfS] -> do
            contents <- readFile filename
            let items = preprocess (T.lines (strip $ T.pack contents))
                minsup = read minsupS :: Double
                minconf = read minconfS :: Double
                wordLst = Prelude.concat items
                counts = M.toList (M.fromListWith (+) (Prelude.map (\x -> (x, 1)) wordLst))
                dataLen = (Prelude.length items) -- dataLen used for minsup and conf calulations
                l1 = Prelude.map (\(a, b) -> ([a], b)) (Prelude.filter (\(_, cnt) -> (getSup cnt dataLen) >= minsup) counts)
                iSets = (itemSets l1 items dataLen minsup)
                support_map = (getSupportMap iSets dataLen)
                alist = Prelude.map (\(x, _) -> x) iSets
                correlations = sortedConfidence (getConfidence alist (M.fromList support_map) minconf)
            -- mapM_ (debugL1) l1
            -- putStrLn " "
            -- mapM_ (debugFreqItems) iSets
            -- putStrLn " "
            mapM_ (debugCor) correlations
         _ -> do
            pn <- getProgName
            die $ "Usage: "++pn++" <filename> <minsup> <minconf>"


debugFreqItems :: ([Text], Int) -> IO()
debugFreqItems (t1, freq) = putStrLn ((show t1) ++ " " ++ (show freq))

debugCor :: ([Text], [Text], Double) -> IO()
debugCor (t1, t2, conf) = putStrLn ((show t1) ++ " " ++ (show t2) ++ " " ++ (show conf))

-- debugL2 :: ([Text]) -> IO()
-- debugL2 xs = putStrLn (show xs)

debugL1 :: ([Text], Int) -> IO()
debugL1 xs = putStrLn (show xs)

preprocess :: [Text] -> [[Text]]
preprocess [] = []
preprocess (x:xs) = (T.splitOn (T.pack ", ") x) : (preprocess xs)

getSup :: Int -> Int -> Double
getSup cnt datalen = (fromIntegral cnt) / (fromIntegral datalen)

getSupportMap :: [([Text], Int)] -> Int -> [([Text], Double)]
getSupportMap isets datalen = Prelude.map (\(x, cnt) -> (x, (getSup cnt datalen))) isets

{--itemsets takes (L_items[k-1], items, and should return = support_map --}
itemSets :: [([Text], Int)] -> [[Text]] -> Int -> Double -> [([Text], Int)]
itemSets [] _ _ _ = []
itemSets prev_L_items items datalen minsup = prev_L_items ++ (itemSets l_items items datalen minsup)
    where
        c_items = aprioriGen prev_L_items
        newItems = getNewItems (c_items) items
        l_items = Prelude.filter (\(_, cnt) -> (getSup cnt datalen) >= minsup) newItems

getNewItems :: [[Text]] -> [[Text]] -> [([Text], Int)]
getNewItems [] [] = []
getNewItems citems baskets =
    M.toList (M.fromListWith (+) (Prelude.map (\x -> (x, 1)) validCombos))
        where
            combos = [(basket, item) | basket <- baskets, item <- citems]
            validCombos = Prelude.map (\(_,i) -> i) (Prelude.filter (\(b, i) -> (Prelude.all (`Prelude.elem` b) i)) combos)

aprioriGen :: [([Text], Int)] -> [[Text]]
aprioriGen items = S.toList(S.fromList([ L.sort (p ++ [(L.last q)]) | p <- prev_L_items, q <- prev_L_items, L.take (L.length p - 1) p == L.take (L.length q - 1) q, (L.last p) /= (L.last q)]))
                  where prev_L_items = Prelude.map (\(x, _) -> x) items

aprioriGenPar :: [([Text], Int)] -> [[Text]]
aprioriGenPar items = S.toList(S.fromList(runEval $ parList (rseq . sort) [(p ++ [(L.last q)]) | p <- prev_L_items, q <- prev_L_items, L.take (L.length p - 1) p == L.take (L.length q - 1) q, (L.last p) /= (L.last q)]))
                  where prev_L_items = Prelude.map (\(x, _) -> x) items

{--
inp = [[T.pack "a", T.pack "b", T.pack "c", T.pack "d"], [T.pack "a", T.pack "b", T.pack "c", T.pack "d", T.pack "e"]]
support_map = fromList [([T.pack "a"], 1), ([T.pack "a", T.pack "b"], 2), 
                        ([T.pack "a", T.pack "b", T.pack "c"], 3),  
                        ([T.pack "a", T.pack "b", T.pack "c", T.pack "d"], 3), 
                        ([T.pack "a", T.pack "b", T.pack "c", T.pack "d", T.pack "e"], 3)]
minconf = fromIntegral 0

do
    hi <- Maybe hi
--}

{--
lfs = [T.pack "a"]
rhs = [T.pack "b", T.pack "c", T.pack "d"]
support_map = fromList [([T.pack "a"], 1), ([T.pack "a", T.pack "b"], 2), ([T.pack "a", T.pack "b", T.pack "c"], 3)]
support_map_empty = fromList []
num = 4
minconf = fromIntegral 0
--}
sortedConfidence :: [([Text], [Text], Double)] -> [([Text], [Text], Double)]
sortedConfidence xs = (sortBy (\(_, _, a) (_, _, b) -> compare b a) xs)

getConfidence :: [[Text]] -> Map [Text] Double -> Double -> [([Text], [Text], Double)]
getConfidence [] _ _  = []
getConfidence xs support_map minconf = xs >>= (\x -> (getConfidence' x support_map minconf))

getConfidence' :: [Text] -> Map [Text] Double -> Double -> [([Text], [Text], Double)]
getConfidence' [] _ _ = []
getConfidence' [_] _ _ = []
getConfidence' (a:ab) support_map minconf
    | isNothing numerator = []
    | otherwise = extractCor [a] ab support_map (fromJust numerator) minconf
    where numerator = M.lookup (a:ab) support_map

extractCor :: [Text] -> [Text] -> Map [Text] Double -> Double -> Double -> [([Text], [Text], Double)]
extractCor _ [] _ _ _ = []
extractCor [] _ _ _ _ = []
extractCor lfs (r:rhs) support_map numerator minconf
    | isNothing lsup = (extractCor (lfs ++ [r]) rhs support_map numerator minconf)
    | conf >= minconf = (lfs, (r:rhs), conf) : (extractCor (lfs ++ [r]) rhs support_map numerator minconf)
    | otherwise = (extractCor (lfs ++ [r]) rhs support_map numerator minconf)
    where 
        lsup = M.lookup lfs support_map
        conf = (numerator / (fromJust lsup))
