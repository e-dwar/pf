module Edot1dot3 where

-- q4

data ArbreBinaire a = Feuille a | Noeud (ArbreBinaire a, ArbreBinaire a)

-- q5

maxArbreBin :: Ord a => ArbreBinaire a -> a

maxArbreBin (Feuille v)    = v
maxArbreBin (Noeud (g, d)) = max (maxArbreBin g) (maxArbreBin d)

-- q6

data ArbreBinaire' a = Feuille' | Noeud' (a, ArbreBinaire' a, ArbreBinaire' a)

-- q7

-- pas de valeur pour le cas de base
-- => il y a 2^2 cas à traiter

maxArbreBin' :: Ord a => ArbreBinaire' a -> a

maxArbreBin' (Noeud' (v, Feuille', Feuille')) = v
maxArbreBin' (Noeud' (v, Feuille', d))        = max v (maxArbreBin' d)
maxArbreBin' (Noeud' (v, g, Feuille'))        = max v (maxArbreBin' g)
maxArbreBin' (Noeud' (v, g, d))               = max v (max (maxArbreBin' g) (maxArbreBin' d))

