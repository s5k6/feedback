> import Tabular
> import Control.Applicative
> import qualified Data.Map as M
> import qualified Data.Set as S
> import Data.Maybe ( catMaybes )
> import Data.List ( intersperse, sort, sortBy )
> import Data.Function ( on )
> import System.Environment ( getArgs )


Parse one rating (rational), check range 0–hi.

> rating :: Rational -> Parser Rational
> rating hi
>   = do p <- getPosition
>        r <- rational
>        assert (r <= hi) $ "Value "++unRat r++" > "++unRat hi++" at "++show p
>        return r


> readPoints :: [Rational] -> String -> IO (M.Map Int [Rational])
> readPoints maxPoints fn
>     = M.fromListWithKey f
>       <$>
>       tableFile' ((,) <$> int <*> (sequence $ map rating maxPoints)) fn
>     where
>     f k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."



Available grades, excluding failure.

> type Grade = String

> grades :: [Grade]
> grades = [ "1.0", "1.3", "1.7"
>          , "2.0", "2.3", "2.7"
>          , "3.0", "3.3", "3.7"
>          , "4.0" -- excluding failure!
>          ]



Available points per task.

> maxPoints :: [Rational]
> maxPoints = [3, 3, 5, 5, 4, 6, 3, 8, 8, 8, 5, 2]

> maxTotal :: Rational
> maxTotal = sum maxPoints



Limit required to gain best grade

> limTopgrade :: Rational

> limTopgrade = fromIntegral . floor $ 0.95 * maxTotal

 > limTopgrade = fromIntegral . floor $ 0.95 * maxTotal



Distance between grades in points (assuming equidistant grades):
 
 > step = 2.5

> step = 3

 > step = (limTopgrade - limTopgrade * 0.5) / fromIntegral (length grades - 1)

 > step = (limTopgrade - maxTotal * 0.5) / fromIntegral (length grades - 1)



Grading table.

> scale :: [(Rational, Grade)]

> scale = zip limits grades
>   where
>     limits = iterate (subtract step) limTopgrade


 > scale
 >   = [ (57.0, "1.0")
 >     , (54.0, "1.3")
 >     , (51.0 -0.5, "1.7")
 >     , (48.0 -0.5, "2.0")
 >     , (45.0 -1, "2.3")
 >     , (42.0 -1, "2.7")
 >     , (39.0 -1.5, "3.0")
 >     , (36.0 -1.5, "3.3")
 >     , (33.0 -2, "3.7")
 >     , (30.0 -2, "4.0")
 >     ]

> ceilingHalf = (/2) . fromIntegral . ceiling . (*2)



Determine grade by points gained.

> grade :: Rational -> Grade
> grade p = head $ (map snd $ dropWhile ((>p) . fst) scale) ++ ["5.0"]


Histogram of values in a map

> histogram :: (Ord a, Ord c) => (b -> c) -> M.Map a b -> M.Map c Int
> histogram f = M.fromList . count . map (f . snd) . M.toList

> count :: Ord a => [a] -> [(a,Int)]
> count = go . sort
>   where
>     go [] = []
>     go (x:xs) = let (as, bs) = span (== x) xs
>                 in (x, length as + 1) : go bs


 
> main
>   = do (a:as) <- getArgs
>        points <- readPoints maxPoints a
>        let grades = M.map (\ps -> let t = sum ps in (t, grade t)) points
>            hist = histogram snd grades

>        putStrLn $ "# maxTotal="++unRat maxTotal
>        putStrLn $ "# step="++unRat step
>        putStrLn $ "# numParticipants=" ++ show (length grades)

>        putStrLn "\n# --- scale ---\n# Pts Grade"
>        mapM_ fmt1 scale

>        putStrLn "\n# --- grades ---\n# ID Pts Grade"
>        mapM_ fmt2 . sortBy (flip (compare `on` (fst . snd))) $ M.toList grades

>        putStrLn "\n# --- histogram ---"
>        mapM_ fmt3 $ M.toList hist

>        return ()
>   where
>   fmt1 (p, g) = putStrLn $ unwords ["#", unRat p, g]
>   fmt2 (mn, (t, g)) = putStrLn $ unwords [show mn, unRat t, g]
>   fmt3 (g, c) = putStrLn $ unwords ["#", g, replicate c '=', '(':show c++")"]





> infix 1 ><
> (><) :: (a -> b) -> (a -> c) -> a -> (b, c)
> f >< g = \x -> (f x, g x)

> infix 1 ?
> (?) :: Bool -> t -> t -> t
> c ? x = \y-> if c then x else y
