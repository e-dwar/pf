module Edot2dot1 where
import Test.QuickCheck

-- q8

decoupe :: [a] -> ([a],[a])

decoupe []       = ([], [])
decoupe [x]      = ([x], [])
decoupe (x:y:xs) = (x : fst p, y : snd p) 
    where 
    p = decoupe xs

-- q9

fusionne :: Ord a => [a] -> [a] -> [a]

fusionne xs []         = xs
fusionne [] ys         = ys
fusionne (x:xs) (y:ys) 
    | x < y     = x : fusionne xs (y : ys)
    | y < x     = y : fusionne (x : xs) ys
    | otherwise = x : y : fusionne xs ys

-- q10

triFusion :: Ord a => [a] -> [a]

triFusion []  = []
triFusion [x] = [x]
triFusion xs  = fusionne ys' zs'
    where
    (ys,zs) = decoupe xs
    ys'     = triFusion ys
    zs'     = triFusion zs

-- q11

inferieurOuEgalAdjacents :: Ord a => [a] -> [Bool]

inferieurOuEgalAdjacents []       = []
inferieurOuEgalAdjacents [x]      = []
inferieurOuEgalAdjacents (x:y:xs) = (y >= x) : inferieurOuEgalAdjacents (y:xs)

inferieurOuEgalAdjacents' :: Ord a => [a] -> [Bool]

inferieurOuEgalAdjacents' []  = []
inferieurOuEgalAdjacents' [x] = []
inferieurOuEgalAdjacents' xs  = zipWith (>=) (tail xs) (init xs)

-- q12

et :: [Bool] -> Bool

et (x:xs) = x && et xs
et _      = True

et' :: [Bool] -> Bool

et' = foldr (&&) True

-- q13

triee :: Ord a => [a] -> Bool

triee = et . inferieurOuEgalAdjacents

-- q14

prop_triFusion :: Ord a => [a] -> Bool

prop_triFusion = triee















