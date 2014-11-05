> import Tabular
> import Control.Applicative
> import qualified Data.Map as M
> import qualified Data.Set as S
> import Data.Maybe ( catMaybes )
> import Data.List ( intersperse )
> import System.Environment ( getArgs )
> import Text.Read ( readMaybe )


> type GroupID = String
> type TutID = String

> data Config
>     = Config
>       { feedback :: String -> String
>       , groups :: String
>       , maxPoints :: String
>       , overview :: String
>       , ratings :: [String]
>       , reqdTotal :: Int -- req'd percent for admittance
>       , reqdEach :: Int -- min percent per exercise
>       , maxLow :: Int -- max failed exercises
>       }
 
 
Store ratings with their position in source.
 
> data Rating = Rating { at :: SourcePos, rat :: Rational }

> instance Show Rating where
>     show r = unwords [ take 5 $ show (fromRational $ rat r :: Float)
>                      , "at"
>                      , show $ at r
>                      ]

Parse one rating (rational), maybe nil, or in the range 0–hi.

> rating :: Rational -> Parser (Maybe Rating)
> rating hi
>   = do i <- mbNil $ pos Rating rational
>        case i of
>          Nothing -> return ()
>          Just p -> do assert (rat p >= 0) $ "Value "++show p++" negative"
>                       assert (rat p <= hi) $ "Value "++show p++" > "++unRat hi
>        return i



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

> mkFeedback :: Config -> [Rational] -> GroupID -> [Maybe Rational]
>          -> IO ()
> mkFeedback cfg maxPoints g ps
>   = writeFile (feedback cfg g) . unlines
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
 

> mkOverview cfg maxPoints limPoints groups ratings
>   = writeFile (overview cfg) . unlines
>     $
>     [ "# Points per student and exercise"
>     , "# (compiled file, do not edit)"
>     , "#"
>     , concat [ "# "
>              , unRat totalReg
>              , " regular points make up 100%.  Including bonuses, "
>              , unRat $ sum limPoints
>              , " points"
>              ]
>     , concat [ "# are available.  To pass, "
>              , show $ reqdTotal cfg
>              , "% = ", unRat reqd
>              , " points are required, and not"
>              ]
>     , concat [ "# more than ", show fMax
>              , " exercises may be rated with less than ", show $ reqdEach cfg, " percent locally."
>              ]
>     , "#"
>     , concat [ "# Columns:  <student> <points>{"
>              , show $ length maxPoints
>              , "} #<passed> <gained>% <margin> <lives>"
>              ]
>     , "#    <student>   ID of the student."
>     , "#    <points>    Gained points; One entry per assignemt."
>     , "#    # the rest of the line is a comment."
>     , "#    <passed>    Whether the student would pass."
>     , "#    <gained>%   Percentage of regular points gained so far."
>     , "#    <margin>    How many points above required limit."
>     , concat [ "#    <lives>     How many more assignments may be rated below "
>              , show $ reqdEach cfg, "%."
>              ]
>     , ""
>     ]
>     ++
>     map f (M.toList $ accumRatings (inverseGroups groups) ratings)
>   where
>   totalReg = sum maxPoints
>   mins = map (\p -> p * fromIntegral (reqdEach cfg) / 100) maxPoints
>   fMax = maxLow cfg
>   reqd = totalReg * fromIntegral (reqdTotal cfg) / 100
>   f (s,ps)
>       = let gained = sum $ catMaybes ps
>             gainedPerc = percent totalReg gained
>             failed = count id $ zipWith (\m -> maybe (0<m) (<m)) mins ps
>             margin = gained - reqd
>             lives = fMax - failed
>         in uncols
>            $
>            s
>            :
>            map (maybe "~" unRat) ps
>            ++
>            [ '#' : if margin >= 0 && lives >= 0 then "pass" else "FAIL"
>            , maybe "0" show gainedPerc ++ "%"
>            , unRat margin
>            , show lives
>            ]


> report cfg
>   = do maxBonusPoints <- tableFile' ((,) <$> rational <*> rational)
>                            (maxPoints cfg)
>        let maxPoints = map fst maxBonusPoints -- the 100%
>            limPoints = map (uncurry (+)) maxBonusPoints -- the limit
>        groups <- readGroupTable $ groups cfg
>        ratings <- joinRatings <$> mapM (readRatingTable limPoints)
>                                   (ratings cfg)
>        assert (M.keysSet ratings `S.isSubsetOf` M.keysSet groups)
>                 "Ratings is not a submap of groups"
>        mkOverview cfg maxPoints limPoints groups ratings
>        mapM_ (uncurry $ mkFeedback cfg maxPoints) . M.toList
>                  $ M.map (fmap $ fmap rat) ratings


> bar arg cfg
>     = case break (=='=') arg of
>         (key, '=':val)
>             -> case key of
>                "feedback"
>                    -> case break (=='%') val of
>                       (d, '%':f) -> cfg{feedback = \g -> d++g++f }
>                       _ -> error "Please specify feedbackDir=path/%/file"
>                "groups" -> cfg{ groups = val }
>                "maxPoints" -> cfg{ maxPoints = val }
>                "overview" -> cfg{ overview = val }
>                "reqdTotal" -> cfg{ reqdTotal = percent }
>                "reqdEach" -> cfg{ reqdEach = percent }
>                "maxLow" -> cfg{ maxLow = integer }
>                _ -> error $ "Unknown key `"++key++"`"
>             where
>             integer
>                 = maybe (error $ "expected: "++key++"::int") id
>                   $ readMaybe val
>             percent
>                 = if integer < 0 || 100 < integer
>                   then error $ "expected: "++key++"::percent"
>                   else integer
>
>         (other,"") -> cfg{ ratings = other : ratings cfg}


> nullCfg = Config{ feedback = error "specify pattern feedback="
>                 , groups = error "specify file groups="
>                 , maxPoints = error "specify file maxPoints="
>                 , overview = error "specify file overview="
>                 , ratings = []
>                 , reqdTotal = error "specify percentage reqdTotal="
>                 , reqdEach = error "specify percentage reqdEach="
>                 , maxLow = error "specify int maxLow="
>                 }
 
 
> main = report =<< foldr bar nullCfg <$> getArgs
