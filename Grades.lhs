> import Tabular
> import Control.Applicative
> import qualified Data.Map as M
> import qualified Data.Set as S
> import Data.Maybe ( catMaybes )
> import Data.List ( intersperse )
> import System.Environment ( getArgs )


 > type GroupID = String
 > type TutID = String

 > data Config
 >     = Config
 >       { groupBaseDir :: String
 >       , feedbackFile :: String
 >       , groupsFile :: String
 >       , maxPointsFile :: String
 >       , studResultFile :: String
 >       , tutorPointFiles :: [String]
 >       , reqdPerc :: Int -- req'd percent for admittance
 >       , failPerc :: Int -- min percent per exercise
 >       , failMax :: Int -- max failed exercises
 >       }
 
 

Parse one rating (rational), check range 0–hi.

> rating :: Rational -> Parser Rational
> rating hi
>   = do p <- getPosition
>        r <- rational
>        assert (r <= hi) $ "Value "++unRat r++" > "++unRat hi++" at "++show p
>        return r



 Parse a file of ratings, make sure each is within the valid range.

 > readRatingTable :: [Rational] -> String -> IO (M.Map GroupID [Maybe Rating])
 > readRatingTable maxPoints t
 >   = M.fromListWithKey f
 >     <$>
 >     tableFile'
 >     ((,) <$> word <*> (sequence $ map rating maxPoints)
 >      <* many (mbNil rational) -- ignore excessive ratings!
 >     )
 >     t
 >   where
 >   f k _ _ = error $ "Group "++show k++" repeated in "++t++"."


 Read the group definitions from a file.  Format is

     <group id> <maybe tutorial> <member>*

 > readGroupTable :: String -> IO (M.Map GroupID (Maybe TutID, [String]))
 > readGroupTable fn
 >   = M.fromListWithKey f
 >     <$>
 >     tableFile'
 >     ((\g s ms -> (g,(s,ms))) <$> word <*> mbNil word <*> many word)
 >     fn
 >     where
 >     f k _ _ = error $ "Group "++show k++" repeated in "++fn++"."



 For each student, determine the groups he is a member of

 > inverseGroups :: M.Map GroupID (a, [String]) -> M.Map String (S.Set GroupID)
 > inverseGroups
 >   = M.fromListWith S.union
 >     .
 >     concatMap (\(g, (_, ms)) -> map (flip (,) $ S.singleton g) ms)
 >     .
 >     M.toList



 Zip two lists of `Maybe`s, where at most one partner is `Just` in each pair.

 > zipDisjoint :: Show a => [Maybe a] -> [Maybe a] -> [Maybe a]
 > zipDisjoint (Just x : _) (Just y : _)
 >   = error $ "Conflicting values " ++ show x ++ ", " ++ show y ++ " found."
 > zipDisjoint (x:xs) (y:ys) = (x <|> y) : zipDisjoint xs ys
 > zipDisjoint xs ys = xs ++ ys


 Join a list of rating tables from different Tutors, make sure no group is
 credited twice for the same assignment.

 > joinRatings :: [M.Map GroupID [Maybe Rating]]
 >             -> M.Map GroupID [Maybe Rating]
 > joinRatings
 >   = M.unionsWith zipDisjoint



 Accumulate student's individual rating, make sure no student is credited twice
 for the same assignment.

 > accumRatings
 >   :: M.Map String (S.Set GroupID)       -- memberships by student
 >   -> M.Map GroupID [Maybe Rating]     -- ratings by group
 >   -> M.Map String [Maybe Rational]  -- ratings by student
 > accumRatings mbs rbg
 >   = M.map (map (fmap rat) . foldl1 zipDisjoint . map (lup rbg) . S.toList) mbs
 >   where
 >   lup k m = maybe [] id $ M.lookup m k



 Generate a report in the group's directory

 > feedback :: Config -> [Rational] -> GroupID -> [Maybe Rational]
 >          -> IO ()
 > feedback cfg maxPoints g ps
 >   = writeFile (groupBaseDir cfg++"/"++g++"/"++feedbackFile cfg) . unlines
 >     $
 >     [ "# Punkte für Gruppe " ++ show g
 >     , unwords [ "# Gesamt:"
 >               , unRat tg
 >               , "von"
 >               , unRat tm
 >               , "(" ++ maybe "~" show (percent tm tg) ++ "%)"
 >               ]
 >     , ""
 >     , "#Blatt\tPunkte\tvon\tProzent"
 >     ]
 >     ++
 >     ( map uncols
 >       $
 >       zipWith (:) (map (pad 2 '0') [1..])
 >       $
 >       zipWith (:) (map (maybe "~" unRat) ps)
 >       $
 >       zipWith (:) (map unRat maxPoints)
 >       $
 >       map (return . maybe "~" (pad 3 ' '))
 >           (zipWith percent maxPoints (map (maybe 0 id) ps))
 >     )
 >     where
 >     tg = sum $ catMaybes ps
 >     tm = sum maxPoints


 > count :: (a -> Bool) -> [a] -> Int
 > count p = length . filter p


 > mkOverview cfg maxPoints groups ratings
 >   = writeFile (studResultFile cfg) . unlines
 >     $
 >     [ "# Points per student and exercise."
 >     , concat [ "# reqd = ", show $ reqdPerc cfg, "% = ", unRat reqd
 >              , "/", unRat maxTotal
 >              , ", extra lives = ", show fMax, ", death = <"
 >              , show $ failPerc cfg
 >              , "%"
 >              ]
 >     , "# Columns: <student> <points>{" ++ show (length maxPoints) ++
 >       "} <passed> # <gained>% <margin> <lives>"
 >     , ""
 >     ]
 >     ++
 >     map f (M.toList $ accumRatings (inverseGroups groups) ratings)
 >   where
 >   maxTotal = sum maxPoints
 >   mins = map (\p -> p * fromIntegral (failPerc cfg) / 100) maxPoints
 >   fMax = failMax cfg
 >   reqd = maxTotal * fromIntegral (reqdPerc cfg) / 100
 >   f (s,ps)
 >       = let gained = sum $ catMaybes ps
 >             gainedPerc = percent maxTotal gained
 >             failed = count id $ zipWith (\m -> maybe (0<m) (<m)) mins ps
 >             margin = gained - reqd
 >             lives = fMax - failed
 >         in uncols
 >            $
 >            s
 >            :
 >            map (maybe "~" unRat) ps
 >            ++
 >            [ if margin >= 0 && lives >= 0 then "pass" else "FAIL"
 >            , "# " ++ maybe "0" show gainedPerc ++ "%"
 >            , unRat margin
 >            , show lives
 >            ]


 > report cfg
 >   = do maxBonusPoints <- tableFile' ((,) <$> rational <*> rational)
 >                            (maxPointsFile cfg)
 >        let maxPoints = map fst maxBonusPoints -- the 100%
 >            limPoints = map (uncurry (+)) maxBonusPoints -- the limit
 >        groups <- readGroupTable $ groupsFile cfg
 >        ratings <- joinRatings <$> mapM (readRatingTable limPoints)
 >                                   (tutorPointFiles cfg)
 >        assert (M.keysSet ratings `S.isSubsetOf` M.keysSet groups)
 >                 "Ratings is not a submap of groups"
 >        mkOverview cfg maxPoints groups ratings
 >        mapM_ (uncurry $ feedback cfg maxPoints) . M.toList
 >                  $ M.map (fmap $ fmap rat) ratings



> readPoints :: [Rational] -> String -> IO (M.Map Int [Rational])
> readPoints maxPoints fn
>     = M.fromListWithKey f
>       <$>
>       tableFile' ((,) <$> int <*> (sequence $ map rating maxPoints)) fn
>     where
>     f k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."




Available grades, excluding failure.

> grades :: [Rational]
> grades = [1.0, 1.3, 1.7, 2.0, 2.3, 2.7, 3.0, 3.3, 3.7, 4.0]

 
> maxPoints :: [Rational]
> maxPoints = [2,2,3,3,2,11,5,7,6,8,12]

> maxTotal :: Rational
> maxTotal = sum maxPoints

> limTopgrade :: Rational
> limTopgrade = 54.9 -- fromIntegral . floor $ 0.9 * maxTotal


Distance between grades in points (assuming equidistant grades):
 
 > step = 2.5

 > step = 3

> step = (limTopgrade - 30) / fromIntegral (length grades - 1)


Limit to get a grade:
 
> limits = iterate (subtract step) limTopgrade

 
Determine grade by points gained

> scale = zip limits grades

> grade p = head $ (map snd $ dropWhile ((>p) . fst) scale) ++ [5.0]




 
 
> main
>   = do (a:as) <- getArgs
>        points <- readPoints maxPoints a
>
>        putStrLn $ "maxTotal="++unRat maxTotal
>        putStrLn $ "step="++unRat step

>        putStrLn "--- scale ---"
>        mapM_ g scale
>
>        putStrLn "--- grades ---"
>        mapM_ f $ M.toList points
>
>        return ()
>   where
>   f (mn, ps) = putStrLn $ unwords [show mn, unRat p, unRat $ grade p]
>       where
>       p = sum ps
>   g (p,v) = putStrLn $ unwords [unRat p, unRat v]
