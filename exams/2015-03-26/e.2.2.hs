module Edot2dot2 where
import Data.Char
import Parser

type Sujet      = String
type Verbe      = String
type Complement = String

data Phrase = SV Sujet Verbe
            | SVC Sujet Verbe Complement
            | SVP Sujet Verbe Phrase
            deriving Show

-- q15

mot :: Parser String

mot = unOuPlus (carCond isLetter) >>= pasQue where 
    pasQue m = if m == "que" then echoue else reussit m

-- q16

espaces :: Parser ()

espaces = do
    unOuPlus (car ' ')
    reussit ()

-- q17

fin :: Sujet -> Verbe -> Parser Phrase

fin s v = do
    car '.'
    return (SV s v)

complement :: Sujet -> Verbe -> Parser Phrase

complement s v = do
    espaces
    m <- mot
    car '.'
    return (SVC s v m)

subordonnee :: Sujet -> Verbe -> Parser Phrase

subordonnee s v = 
    let motQue m = if m /= "que" then echoue else reussit () in
    let que      = unOuPlus (carCond isLetter) >>= motQue in do
    espaces
    que
    espaces
    s' <- mot
    espaces
    v' <- mot
    p <- (fin s' v' ||| complement s' v' ||| subordonnee s' v')
    return (SVP s v p)
    
phrase :: Parser Phrase

phrase = do
    s <- mot
    espaces
    v <- mot
    fin s v ||| subordonnee s v
    





















