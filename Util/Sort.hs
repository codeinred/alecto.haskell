module Util.Sort where


merge [] ls = ls
merge ls [] = ls
merge (x:xs) (y:ys) = if x < y
    then x : merge xs (y:ys)
    else y : merge (x:xs) ys

evens [] = []
evens (x:xs) = x : odds xs
odds [] = []
odds (x:xs) = evens xs
sort [] = []
sort [x] = [x]
sort [x,y] = if x < y then [x,y] else [y,x]
sort ls = merge first second where
    first = sort $ evens ls
    second = sort $ odds ls

mergeOn f [] ls = ls
mergeOn f ls [] = ls
mergeOn f (x:xs) (y:ys) = if f x < f y
    then x : mergeOn f xs (y:ys)
    else y : mergeOn f (x:xs) ys
sortOn f [] = []
sortOn f [x] = [x]
sortOn f [x,y] = if f x < f y then [x,y] else [y,x]
sortOn f ls = mergeOn f first second where
    first = sortOn f $ evens ls
    second = sortOn f $ odds ls
