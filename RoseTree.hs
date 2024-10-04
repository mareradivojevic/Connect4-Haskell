module RoseTree
(   Rose(..),
    size,
    height,
    leavesCount,
    leaves,
    elemsOnDepth,
    nodes,
    foldRose,
    generateRose
) where

-- struktura rose
data Rose a = Node a [Rose a] deriving (Show)

-- a) size - vraća broj čvorova stabla
size :: Rose a -> Int
size (Node n children) = 1 + foldl (\acc child -> acc + size child) 0 children
-- size (Node n children) = 1 + foldr ((+) . size) 0 children

-- height - računa visinu stabla, odnosno najdužu putanju (broj grana) od korena do lista
height :: Rose a -> Int
height (Node n []) = 0
height (Node n children) = 1 + maximum (map height children)

-- b) leavesCount - vraća broj listova,
leavesCount :: Rose a -> Int
leavesCount (Node n []) = 1
leavesCount (Node n children) = foldl (\acc child -> acc + leavesCount child) 0 children
-- leavesCount (Node n children) = foldr ((+) . leavesCount) 0 children

-- leaves - vraća listu koja sadrži vrednosti svih listova stabla
leaves :: Rose a -> [a]
leaves (Node n []) = [n]
leaves (Node n children) = foldl (\acc child -> acc ++ leaves child) [] children
-- leaves (Node n children) = foldr ((++) .  leaves) [] children

-- c) elemsOnDepth - vraća vrednosti svih elemenata na određenoj dubini
elemsOnDepth ::  Rose a -> Int -> [a]
elemsOnDepth (Node n children) d
    | d == 0 = [n]
    | null children = []
    | otherwise = foldl (\acc child -> acc ++ elemsOnDepth child (d-1)) [] children
--  | otherwise = foldr ((++) . elemsOnDepth (d-1)) [] children

-- d) instancirati tipsku klasu Functor za tip podataka Rose
instance Functor Rose where
    fmap f (Node n children) = Node (f n) (map (fmap f) children)

-- e) napisati funkciju foldRose koja izvršava fold (levi ili desni) na svim čvorovima stabla tipa Rose (na
-- primer ako imamo stablo sa celim brojevima i prosledimo funkciju (+) i akumulator 0 kao rezultat se
-- -- vraća zbir svih čvorova)
nodes :: Rose a -> [a]
nodes (Node n []) = [n]
nodes (Node n children) = n : concat (map nodes children)

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc roseTree = foldl f acc (nodes roseTree)

-- f) generateRose - funkcija koja generiše stablo na osnovu zadatog korena, funkcije koja preslikava
-- element na listu elemenata i broj koraka (dubinu rezultujućeg stabla)
generateRose :: (a -> [a])-> Int -> a -> Rose a
generateRose f 0 root = Node root []
generateRose f d root = Node root (map (generateRose f (d-1)) (f root))