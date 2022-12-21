import Control.Parallel.Strategies(rpar, parMap)
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
                dataLen = (Prelude.length items)
                --loaded argument data
                wordLst = Prelude.concat items
                counts = M.toList (M.fromListWith (+) (Prelude.map (\x -> (x, 1)) wordLst))
                l1 = Prelude.map (\(a, b) -> ([a], b)) (Prelude.filter (\(_, cnt) -> (getSup cnt dataLen) >= minsup) counts)
                --construct L1 itemset
                iSets = (itemSets l1 items dataLen minsup)
                --obtained Lk itemsets
                support_map = (getSupportMap iSets dataLen)
                --generate support map
                correlations = sortedConfidence (getConfidencePar (Prelude.map (\(x, _) -> x) iSets) (M.fromList support_map) minconf)
                --obtain correlations
            mapM_ (printCor) correlations
         _ -> do
            pn <- getProgName
            die $ "Usage: "++pn++" <filename> <minsup> <minconf>"

---HELPER DATA PREPROCESSING FNS---
printCor :: ([Text], [Text], Double) -> IO()
printCor (t1, t2, conf) = putStrLn ((show t1) ++ " " ++ (show t2) ++ " " ++ (show conf))

preprocess :: [Text] -> [[Text]]
preprocess [] = []
preprocess (x:xs) = (T.splitOn (T.pack ",") x) : (preprocess xs)


---SUPPORT MAP CONSTRUCTION---
getSup :: Int -> Int -> Double
getSup cnt datalen = (fromIntegral cnt) / (fromIntegral datalen)

getSupportMap :: [([Text], Int)] -> Int -> [([Text], Double)]
getSupportMap isets datalen = Prelude.map (\(x, cnt) -> (x, (getSup cnt datalen))) isets


---ITEM SET CONSTRUCTION---
itemSets :: [([Text], Int)] -> [[Text]] -> Int -> Double -> [([Text], Int)]
itemSets [] _ _ _ = []
itemSets prev_L_items items datalen minsup = prev_L_items ++ (itemSets l_items items datalen minsup)
    where
        c_items = aprioriGenPar prev_L_items
        l_items = Prelude.filter (\(_, cnt) -> (getSup cnt datalen) >= minsup) (prunePar (c_items) items)


--  PRUNING    --
prune :: [[Text]] -> [[Text]] -> [([Text], Int)]
prune [] [] = []
prune citems baskets = Prelude.map (\item -> (item, supCount item baskets)) citems

prunePar :: [[Text]] -> [[Text]] -> [([Text], Int)]
prunePar [] [] = []
prunePar citems baskets = parMap rpar (\item -> (item, supCount item baskets)) citems

supCount :: [Text] -> [[Text]] -> Int
supCount _ [] = 0
supCount item (basket:xs)
    | (Prelude.all (`Prelude.elem` basket) item) = 1 + (supCount item xs)
    | otherwise = supCount item xs


-- CANDIDATE GENERATION ---

removeDups :: [[Text]] -> [[Text]]
removeDups xs = S.toList(S.fromList(xs))

aprioriGenPar :: [([Text], Int)] -> [[Text]]
aprioriGenPar items = removeDups (Prelude.concat((parMap rpar (\x -> (createCand x prev_L_items)) prev_L_items)))
                    where prev_L_items = Prelude.map (\(x, _) -> x) items

aprioriGen :: [([Text], Int)] -> [[Text]]
aprioriGen items = removeDups (Prelude.concat((Prelude.map (\x -> (createCand x prev_L_items)) prev_L_items)))
                    where prev_L_items = Prelude.map (\(x, _) -> x) items

createCand :: [Text] -> [[Text]] -> [[Text]]
createCand p prev_L_items = [L.sort (p ++ [(L.last q)]) | q <- prev_L_items, L.take (L.length p - 1) p == L.take (L.length q - 1) q, (L.last p) /= (L.last q)]


-- CONFIDENCE RELATIONS MINING ---

sortedConfidence :: [([Text], [Text], Double)] -> [([Text], [Text], Double)]
sortedConfidence xs = (sortBy (\(_, _, a) (_, _, b) -> compare b a) xs)

getConfidencePar :: [[Text]] -> Map [Text] Double -> Double -> [([Text], [Text], Double)]
getConfidencePar [] _ _  = []
getConfidencePar xs support_map minconf = Prelude.concat (parMap rpar (\x -> (getConfidence' x support_map minconf)) xs)

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
