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
>   = M.fromListWithKey conflict
>     <$>
>     tableFile' ((,) <$> int <*> (sequence $ map rating maxPoints)) fn
>   where
>     conflict k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."


> readReg :: String -> IO (M.Map Int String)
> readReg fn = M.fromListWithKey conflict <$> tableFile' ((,) <$> int <*> word) fn
>   where
>     conflict k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."

> readAdmission :: String -> IO (M.Map String Bool)
> readAdmission fn
>   = M.fromListWithKey conflict
>     <$>
>     tableFile' ((,) <$> word <*> pass <* perc <* rational <* many (mbNil rational)) fn
>   where
>     conflict k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."
>     pass = (key "pass" >> return True) <|> (key "fail" >> return False)

> readOldAdmission :: String -> IO (M.Map String Bool)
> readOldAdmission fn
>   = M.fromListWithKey conflict
>     <$>
>     tableFile' ((,) <$> word <* rational <*> pass) fn
>   where
>     conflict k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."
>     pass = (key "pass" >> return True) <|> (key "fail" >> return False)

> readGrades :: String -> IO (M.Map Int String)
> readGrades fn
>   = M.fromListWithKey conflict
>     <$>
>     tableFile' ((,) <$> int <* rational <*> (unRat <$> rational)) fn
>   where
>     conflict k _ _ = error $ "MatkNr "++show k++" repeated in "++fn++"."

> main = do
>     regKdp <- readReg "/home/sk/uni/teach/inf2_16s/exam/studis/anmeld_kdp_1.csv"
>     regPk2 <- readReg "/home/sk/uni/teach/inf2_16s/exam/studis/anmeld_pk2_1.csv"
>     grades <- readGrades "/home/sk/uni/teach/inf2_16s/exam/results/grades1.csv"
>     admission <- M.unionsWith (||)
>                  <$>
>                  sequence
>                  ( readAdmission "/home/sk/uni/teach/inf2_16s/misc/sk/punktestand"
>                  : map readOldAdmission
>                    [ "/home/sk/uni/teach/inf2_16s/misc/sk/old_admissions/inf2_13s"
>                    , "/home/sk/uni/teach/inf2_16s/misc/sk/old_admissions/inf2_14s"
>                    , "/home/sk/uni/teach/inf2_16s/misc/sk/old_admissions/inf2_15s"
>                    ]
>                  )

>     let f mn = ( mn, toStudIS ad rp rk eg, (ad,rp,rk,eg) )
>           where
>             Just ea = M.lookup mn regKdp <|> M.lookup mn regPk2 <|> err ["unknown", show mn]
>             ad = maybe False id $ M.lookup ea admission
>             rk = maybe False (\n-> n==ea ? True $ err ["mismatch", n, ea]) $ M.lookup mn regKdp
>             rp = maybe False (\n-> n==ea ? True $ err ["mismatch", n, ea]) $ M.lookup mn regPk2
>             eg = M.lookup mn grades
>
>     putStrLn "# MatkNr PK2 KdP # (Zul, RegPk2, regKdp, Klausur)"
>     mapM_ fmt1 . map f $ S.toList (M.keysSet regKdp `S.union` M.keysSet regPk2)

>   where
>     fmt1 (mn, (pk2,kdp), args)
>       = putStrLn $ unwords [show mn, pk2, kdp, "#", show args]



> --      zul   regPk2 regKdp exam    ->   PK2                         KdP
> toStudIS True  True   True  (Just g)  = (read g < 5.0 ? "BE" $ "NB", g    )
> toStudIS True  True   True  Nothing   = ("NE"                      , "NE" )
> toStudIS True  True   False Nothing   = ("RM"                      , "~" )
> toStudIS True  False  True  (Just g)  = (read g < 5.0 ? "BE" $ "NB", g    )
> toStudIS True  False  True  Nothing   = ("~"                       , "NE" )
> toStudIS False True   True  Nothing   = ("NZ"                      , "NZ" )
 
 > toStudIS True  True  False  (Just g)  = (read g < 5.0 ? "BE" $ "NB", "~"  )

> toStudIS zul  regPk2 regKdp exam
>   = err [ "Dunno:"
>         , show zul, show regPk2, show regKdp, show exam
>         ]



> err = error . unwords

> nl :: Show a => Maybe a -> [Char]
> nl = maybe "~" show


> infix 1 ><
> (><) :: (a -> b) -> (a -> c) -> a -> (b, c)
> f >< g = \x -> (f x, g x)

> infix 1 ?
> (?) :: Bool -> t -> t -> t
> c ? x = \y-> if c then x else y
