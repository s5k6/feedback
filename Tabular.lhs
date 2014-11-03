
A module for parsing table-shaped files. A table is a sequence of lines, empty
lines are ignored. A comment (we use the #-to-eol style) constitutes the end of
a line. A line contains space/tab-separated values.

The following parsers are predefined. See the example section at the very end
about how to combine these.

The field parsers                                               M-x orgtbl-mode

  | Parser   | Type     | Description                                   |
  |----------+----------+-----------------------------------------------|
  | integer  | Integer  | a non-empty sequence of digits                |
  | int      | Int      | optionally started by + or -                  |
  |----------+----------+-----------------------------------------------|
  | rational | Rational | An Integer Ratio, may be specified as decimal |
  |          |          | fraction 0.4, or with a slash 2/5, both       |
  |          |          | representing the same value. Optional sign    |
  |          |          | allowed as Prefix                             |
  |----------+----------+-----------------------------------------------|
  | double   | Double   | Inexact versions of rational.                 |
  | float    | Float    |                                               |
  |----------+----------+-----------------------------------------------|
  | str      | String   | A String enclosed in double quotes, escape    |
  |          |          | sequences currently include \t, \n, \\, \".   |
  |----------+----------+-----------------------------------------------|
  | word     | String   | A String not enclosed in double quotes. No    |
  |          |          | escaping allowed. Must start with a letter    |
  |----------+----------+-----------------------------------------------|
  | bln      | Boolean  | The word "True" or "false".                   |
  |----------+----------+-----------------------------------------------|
  | nil      | ()       | The word "~".                                 |
  |----------+----------+-----------------------------------------------|
  | key x    | ()       | Expects the given keyword x, e.g., key "foo". |
  |----------+----------+-----------------------------------------------|
  | pos f p  | a        | Applies `f` to SourcePos and value from p     |
  |----------+----------+-----------------------------------------------|


================================================================================


> module Tabular ( table, tableFile, tableFile'
>                , bln, nil, key
>                , int, integer, rational, double, float
>                , str, word
>                , pos, getPosition
>                , mbNil
>                , spaces, lexeme
>                , Parser, SourcePos
>                , uncols, unRat, assert, percent, pad  
>                ) where


> import Text.ParserCombinators.Parsec hiding ( (<|>), many, spaces, space )
> import Control.Applicative
> import Data.Char ( digitToInt )
> import Data.Ratio
> import Data.List ( intersperse )


--------------------------------------------------------------------------------
Spaces, delimiters and newlines


> space = oneOf " \t" -- not newline!
> spaces = many space

> lexeme :: Parser a -> Parser a
> lexeme p = p <* spaces

> nl = lexeme newline >> return ()
> comment = lexeme $ char '#' >> manyTill anyChar newline >> return ()
> eol = many1 $ comment <|> nl


--------------------------------------------------------------------------------
Helper parsers


Parse an optional sign, i.e., ‘+’, ‘-’, or nothing defaulting to ‘+’.

> sign :: Num a => Parser (a -> a)
> sign = choice [char '-' >> return negate, char '+' >> return id, return id]


Parse a non-empty sequence of digits.

> digits1 :: Parser [Int]
> digits1 = map digitToInt <$> many1 digit


Given a base, interpret the list of numbers as integral part of a fraction.

> integral :: Num a => a -> [a] -> a
> integral b = foldl (\ n d -> b*n+d) 0


Given a base, interpret the list of numbers as fractional part of a fraction.

> places :: Fractional b => b -> [b] -> b
> places b = foldr (\ d n -> (d+n)/b) 0


--------------------------------------------------------------------------------
Parsing a table


> table :: Parser a -> Parser [a]
> table l = sepEndBy l eol


‘tableFile rec fn’

> tableFile :: Parser a -> String -> IO (Either ParseError [a])
> tableFile l fn
>     = parseFromFile p fn
>     where
>     p = do spaces
>            many $ comment <|> nl
>            t <- table l
>            eof
>            return t



This version of ‘tableFile'’ resorts to ‘error’ on parse error.

> tableFile' :: Parser a -> String -> IO [a]
> tableFile' l fn = either (error . show) id <$> tableFile l fn


--------------------------------------------------------------------------------
Some predefined field parsers


Parse a boolean.

> bln :: Parser Bool
> bln = (key "True" >> return True) <|> (key "False" >> return False)



Parse a decimal Int.

> int :: Parser Int
> int = lexeme $ sign <*> (integral 10 <$> digits1)



Parse a decimal Integer.

> integer :: Parser Integer
> integer = lexeme $ sign <*> (integral 10 . map toInteger <$> digits1)



Parse a decimal Relational. Digests ‘+1.23’ and ‘-2/5’.

> rational :: Parser Rational
> rational
>     = lexeme
>       $
>       sign
>       <*>
>       do n <- integral 10 . map toRational <$> digits1
>          choice [ char '.' >> (n+) . places 10 . map toRational <$> digits1
>                 , char '/' >> (n/) . integral 10 . map toRational <$> digits1 
>                 , return $ n
>                 ]


Parse a Double value. Digests the same as ‘rational’.

> double :: Parser Double
> double = fromRational <$> rational

> float :: Parser Float
> float = fromRational <$> rational


Parse a string. Currently, only the escape sequences \", \\, \n, and \t are
recognised.

> str :: Parser String
> str = lexeme $ between (char '"') (char '"') $ many c
>     where
>     c = choice [noneOf "\\\"", char '\\' >> e]
>     e = choice [char '\\', char '"', char 'n' >> return '\n', char 't' >> return '\t']



Parse a Nil value.

> nil :: Parser ()
> nil = lexeme $ char '~' >> return ()


Shorthand for Nil-or-Something parsers.

> mbNil :: Parser a -> Parser (Maybe a)
> mbNil p = (const Nothing <$> nil) <|> (Just <$> p)



Parse a given keyword.

> key :: String -> Parser ()
> key k = lexeme $ string k >> notFollowedBy alphaNum >> return ()



Parse a word, i.e., a String not delimited by double quotes

> word :: Parser String
> word = lexeme $ (:) <$> letter <*> many (noneOf " \t\n\\\"")



Parse something, and augment it with the position in the source code.

> pos :: (SourcePos -> a -> b) -> Parser a -> Parser b
> pos f p = f <$> getPosition <*> p


--------------------------------------------------------------------------------
Helper functions

       
> assert p msg
>   = if p then return () else fail msg


> uncols = concat . intersperse "\t"        

 
> unRat = show . fromRational

  
> pad n c
>   = reverse . fill n . reverse . show
>   where
>   fill 0 xs = xs
>   fill k (x:xs) = x : fill (k-1) xs
>   fill k [] = replicate k c

    
> percent :: Rational -> Rational -> Maybe Int
> percent all frac
>   = if all > 0
>     then Just . floor . fromRational $ frac / all * 100
>     else Nothing

         
================================================================================
Example section


Parsing a table of type  String × Integer

> test0 fn = tableFile' ((,) <$> str <*> int) fn



Parsing a table of type  String × (Integer | Nil)

> test1 fn = tableFile' ((,) <$> str <*> mbNil int) fn



Abusing the idea: Parsing records which are of one of the following two forms,
as indicated by the keyword:

    good <str> <bln> <int>
    bad <int> <int>

Note that the tuple-construction has the same return type for each case.

> test2 = tableFile'
>         $
>         (key "bad" >> (\a b -> (show b, False, a)) <$> int <*> int)
>         <|>
>         (key "good" >> (,,) <$> str <*> bln <*> int)


================================================================================
