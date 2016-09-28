
Template haskell snippet to add help text into compiled program.

> {-# LANGUAGE TemplateHaskell #-}
>
> module Literal where
>                
> import Language.Haskell.TH
>
> help = $( runIO (readFile "help.txt") >>= stringE )
