doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
    then x
    else x*2

{-Function on types vars that have successor:
 - succ
 -
 - -}


 {-Functions on two vars: (can also be used as "a `function` b" )
  - min
  - max
  -
  - -}

  {-Function definitions:
   - function_name [parameter]* = expression
   - -}
doubleSmallNumber' x = (if x > 100 then x else x * 2) +1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- [Char] can be written as String too!
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

{-List functions:
 - head
 - tail
 - init
 - last
 - length
 - null
 - reverse
 - take
 - drop
 - maximum
 - minimum
 - product
 - sum
 - elem
 - zip list1 list2
 - -}

{-Tuple functions:
 - fst
 - snd
 - 
 - -}

{-Comments can
be written like
this too-}

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

{-Types in Haskell:
 - Int
 - Integer
 - Float
 - Double
 - Char
 - String
 - ()
 - []
 - Bool
 - Ordering
 - -}

 {-Typeclasses in Haskell:
  - Eq
  - Ord
  - Show
  - Read
  - Enum ((),Bool,Char,Ordering,Int,Integer,Float,Double)
  - Bounded
  - Num (Int, Integer, Float, Double)
  - Integral (Int, Integer)
  - Floating (Float, Double
  - RealFrac
  - RealFloat
  -
  - -}

{-Other functions:
 - show
 - read
 - minBound
 - maxBound
 - fromIntegral
 - -}

--Syntax in Functions
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

recurseFactorial :: (Integral a) => a -> a
recurseFactorial 0 = 1
recurseFactorial n = n * recurseFactorial (n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

--bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"

verse 0   = "No more bottles of beer on the wall, no more bottles of beer.\n"
    ++ "Go to the store and buy some more, 99 bottles of beer on the wall."

verse n   = bottles n ++ " of beer on the wall, " ++ bottles n ++ " of beer.\n"
    ++ "Take one down and pass it around, " ++ bottles (n-1)
    ++ " of beer on the wall.\n"

singBottles = mapM (putStrLn . verse) [99,98..0]

addVectors :: (Num a ) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 +y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x --could be also [x]
tell (x:y:[]) = "The list has two elements " ++ show x ++" and " ++ show y --[x,y]
tell (x:y:_) = "The list is too long. First are " ++ show x ++" and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--as patterns ( nameIt@(pattern) )
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++" is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

--Function can be defined with backticks, not just called
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny
        = "BMI: "++ show bmi ++ " You're underweight, you emo, you!"
    | bmi <= normal
        = "BMI: "++ show bmi ++ " You're normal, but probably ugly."
    | bmi <= fat    
        = "BMI: " ++ show bmi ++ " You're fat! Loose some wight, fatty!"
    | otherwise
        = "BMI: " ++ show bmi ++ " You are a whale, congrats!"
    where
        bmi     = weight / height ^2
        skinny  = 15.5
        normal  = 25.0
        fat     = 30.0
        --could do (skinny, normal, fat) = (18.5, 25.0, 30.0)

--Function initials returns "A. B." when: initials "Adolf" "Brown"  
initials :: String -> String -> String
--Method 1 - using functions
--initials firstName lastName = head firstName : '.':' ' : head lastName : "."
--Method 2 - using list collations
--initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."
--Method 3 - using head attaching :)
--initials (f:_) (l:_) = f : '.':' ':l:"."
--Method 4 - combination of methods 2 and 3
--initials (f:_) (l:_) = (f:". ") ++ (l:".")
--Method 5 - pattern matching in WHERE bindings
initials firstName lastName = (f:". ") ++ (l:".")
    where
        (f:_) = firstName
        (l:_) = lastName

--Function calcBmis returns a list of bmi, when supplied
--with list weight, height tuples
calcBmis ::(RealFloat a) => [(a,a)] -> [a]
{- --with helper function
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight/height ^2
-}
--calcBmis xs = [w/h^2|(w,h) <- xs] --without helper function
calcBmis xs  = [bmi | (w,h) <- xs, let bmi = w/h^2] --using let

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 1 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


--Case expressions

head'' :: [a] -> a
{-
head' [] = error "No head for empty lists!"
head' (x:_) = x
-}
head'' xs = case xs of
    []      -> error "No head for empty lists!"
    (x:_)   -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where
        what []     = "empty."
        what [x]    = "a singleton list."
        what xs     = "a longer list."




