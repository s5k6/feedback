> {-# OPTIONS_GHC -XTemplateHaskell #-}
>
> module Literal where
>                
> import Language.Haskell.TH
>
> help = $( runIO (readFile "help.txt") >>= stringE )
