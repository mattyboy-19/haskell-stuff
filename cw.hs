-- 19COA108 Haskell Coursework 2019, M.Peters

---- Part 1 ----

-- 1. row function takes the input for the length of rows and outputs the 
--    required number of stars
-- 2. rectangle calls row the number of times needed (x)
-- 3. steps calls rectangle z times, use y*z as input to get the steps the right length, i.e. on the second #
--    iteration we double the length, we call steps recursively first then the rectangle to get it top to bottom

-- steps function to output steps of rectangles
steps :: Int -> Int -> Int -> String
steps x y z 
 | z == 0 = " "
 | z > 1 = steps x y (z-1) ++ rectangle x (y*z)
 | z == 1 = rectangle x (y*z) 
 
-- row function to output lines of *  
row :: Int -> String
row y
 | y == 0 = " "
 | y>1 = "*" ++ row(y-1)
 | y == 1 = "*"

-- rectangle function to output rows on different lines
rectangle :: Int -> Int -> String
rectangle x y
 | x == 0 = " "
 | x>1 = row y ++ "\n" ++ rectangle(x-1) y 
 | x == 1 = row y ++ "\n"


---- Part 2 ----

-- 1. Verify that the value is 5 or more
-- 2. define a list of coordinates that make a n-1 x n-1 square
--    (starting at 0)
-- 3. match values in coordinate list with * in other list, e.g.
--    when x=y or when y+x =n-1, x,y=0/n-1. " " for character that does not make shape.
--    fst takes first in tuple, snd takes second. e is uses as a pointer to go thorugh each coordinate.
-- 4. print new string with n elements per row.
--    1.if the string has length of less than n then print the string.
--    2. otherwise, take the first n of a string add a newline, and call recursively on the rest.

-- function to check if size is allowed
flagpattern :: Int -> String
flagpattern n
 | n < 5 = "Please enter a number greater than or equal to 5"
 | otherwise = addnewline (flagrow (coordinates n) 0 n) n

-- get list of coordinates
coordinates :: Int -> [(Int,Int)]
coordinates n = [(x,y) | x <- [0..(n-1)], y <- [0..(n-1)]]

-- populate each coordinate with either * or ' ' 
-- runs while e (count) is less than n^2 (number of cooridnate points)
flagrow :: [(Int,Int)] -> Int -> Int -> String
flagrow cl e n
 | e < n^2 = 
  if fst(cl!!e) == snd(cl!!e) || fst(cl!!e) + snd(cl!!e) == n-1 
     || (fst(cl!!e) == 0 || snd(cl!!e) == 0)
     || (fst(cl!!e) == n-1 || snd(cl!!e) == n-1) 
  then "*" ++ flagrow cl (e+1) n
  else " " ++ flagrow cl (e+1) n
 | otherwise = ""

-- add a newline every n elements
addnewline :: String -> Int -> String
addnewline xs n = 
 if length xs <= n
 then xs
 else take n xs ++ "\n" ++ addnewline (drop n xs) n

---- Part 3 ----

-- 1. If either words are empty, return empty
-- 2. If the string (s) is less than the word, return s
-- 3. if the first word matches (by taking the length of the word from the string), add the second
--    word and apply swapwords on the string with the first word dropped (remove characters up to length of word)
-- 4. If it is not the first part, apply it on the tail

swapwords :: String -> String -> String -> String
swapwords w1 w2 s
  | w1 == "" && w2 == "" = ""
  | length s < length w1 = s  
  | w1 == take (length w1) s = w2 ++ swapwords w1 w2 (drop (length w1) s)
  | otherwise = x : swapwords w1 w2 xs
  where 
    x:xs = s

---- Part 4 ----

-- 1. call function that eliminates like characters on each
--    string. elimchars filters all the elements that are in s1 from s2. notElem a s1 returns true for
--    elements not in s1, we can then filter them with s2 to get the required string
--    elimchars uses a, an anonymous function that takes a and s1 as arguments (lambda function)
-- 2. filter(/=' ') removes all the blank spaces from the list produced from elimchars.
-- 3. then call lphi function to count through the letter and end on a value. Use modulo to see what value we end
--    up on, e.g. = 1 is l 2 is p etc.

-- function that checks if they are not empty, if not empty then calls lphi on the filtered version of elimchars
compatibility :: String -> String -> String
compatibility s1 s2 
 | s1 == "" || s2 == "" = ""
 | otherwise = lphi s1 s2 (filter (/=' ') (elimchars s2 s1)) ++ " and " 
	       ++ lphi s2 s1 (filter (/=' ') (elimchars s1 s2))

-- eliminates characters in s2 that are in s1
elimchars :: String -> String -> String
elimchars s1 s2 = filter (\a -> notElem a s1) s2

-- use modulo to find which lphi value is assigned
lphi :: String -> String -> String -> String
lphi s1 s2 sR
 | length sR `mod` 4 == 1 = s1 ++ " loves " ++ s2
 | length sR `mod` 4 == 2 = s1 ++ " is physical with " ++ s2
 | length sR `mod` 4 == 3 = s1 ++ " hates " ++ s2
 | length sR `mod` 4 == 0 = s1 ++ " is indifferent to " ++ s2

---- Part 5 ----

-- 1. Define the polymorphic type, takes a list and an integer and returns a list
-- 2. if the list is empty return an empty list
-- 3. if the head matches c call split on the tail
-- 4. if the head does not match add the head to calling split on the tail
split :: (Eq a) => [a] -> a -> [a]
split [] _ = []
split (x:xs) c 
 | x == c = split xs c 
 | otherwise = [x] ++ split xs c
 

