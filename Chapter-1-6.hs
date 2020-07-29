-- This exercise covers the first 6 chapters of "Learn You a Haskell for Great Good!"
-- Download this file and then type ":l Chapter-1-6.hs" in GHCi to load this exercise
-- Some of the definitions are left "undefined", you should replace them with your answers.

-- Answered by Matthew Chen: edcr1790@gmail.com

list1 = [1,2,3,4,5,6,7]
list2 = "Hello world!"
list3 = "E"

-- Find the penultimate (second-to-last) element in list xs
penultimate xs = last (init xs)

-- Find the antepenultimate (third-to-last) element in list xs
antepenultimate xs = xs !!  (length(xs) -3)
-- another: antepenultimate xs = last( init( last(init xs)))

-- Left shift list xs by 1
-- For example, "shiftLeft [1, 2, 3]" should return "[2, 3, 1]"
shiftLeft xs = tail(xs) ++ [head(xs)]

-- Left shift list xs by n
-- For example, "rotateLeft 2 [1, 2, 3]" should return "[3, 1, 2]"
rotateLeft n xs = (iterate shiftLeft xs) !! n
-- another: rotateLeft n xs = drop n (take (n+length(xs))  (cycle xs))

-- Insert element x in list xs at index k
-- For example, "insertElem 100 3 [0,0,0,0,0]" should return [0,0,0,100,0,0]
insertElem x k xs = take (k-1) xs ++ [x] ++ drop k xs

-- Here we have a type for the 7 days of the week
-- Try typeclass functions like "show" or "maxBound" on them
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
         deriving (Eq, Ord, Show, Bounded, Enum)

-- Note that if you try "succ Sun", you should get an error, because "succ" is not defined on "Sun"
-- Define "next", which is like "succ", but returns "Mon" on "next Sun"
next :: Day -> Day
next day | day == Sun = Mon | otherwise= succ day             -- Because we have given input with explicit declaration, the "otherwise" wil be included in Day.
                                                                                               -- Random input results in error.

-- Return "True" on weekend
isWeekend :: Day -> Bool
isWeekend day = if day `elem` [Sat, Sun] then True else False

data Task = Work | Shop | Play deriving (Eq, Show)

-- You are given a schedule, which is a list of pairs of Tasks and Days
schedule :: [(Task, Day)]
schedule = [(Shop, Fri), (Work, Tue), (Play, Mon), (Play, Fri)]

-- However, the schedule is a mess
-- Sort the schedule by Day, and return only a list of Tasks.
-- If there are many Tasks in a Day, you should keep its original ordering
-- For example, "sortTask schedule" should return "[(Play, Mon), (Work, Tue), (Shop, Fri), (Play, Fri)]"
sortTask :: [(Task, Day)] -> [(Task, Day)]
sortTask [] = []
-- let's try recursive: quicksort
sortTask (xs:ys) =
                let smallTask = sortTask (filter  ((snd xs >).  snd  ) ys)
                    bigTask = sortTask (filter  ((snd xs <=).  snd ) ys)
                in smallTask ++ [xs] ++ bigTask

-- This function converts days to names, like "show", but a bit fancier
-- For example, "nameOfDay Mon" should return "Monday"
nameOfDay :: Day -> String
nameOfDay x =  case x of
                                       Mon -> "Monday"
                                       Tue -> "Tuesday"
                                       Wed -> "Wednesday"
                                       Thu -> "Thursday"
                                       Fri -> "Friday"
                                       Sat -> "Saturday"
                                       Sun -> "Sunday"

-- You shouldn't be working on the weekends
-- Return "False" if the Task is "Work" and the Day is "Sat" or "Sun"
labourCheck :: Task -> Day -> Bool
labourCheck task day | task == Work && day `elem` weekend = False
                                    | otherwise = True
                                    where weekend = [Sat, Sun]

-- Raise x to the power y using recursion
-- For example, "power 3 4" should return "81"
power :: Int -> Int -> Int
power x y  | y == 0 = 1
                  | y < 0 =  errorWithoutStackTrace "Negative exponent!\n" --cause output need to be an interger
                  | otherwise = x * power x (y-1)

-- Convert a list of booleans (big-endian) to a interger using recursion
-- For example, "convertBinaryDigit [True, False, False]" should return 4
convertBinaryDigit :: [Bool] -> Int
convertBinaryDigit  [] = 0
convertBinaryDigit bits =  2* convertBinaryDigit(init(bits)) + (if last(bits) then 1 else 0)

-- Create a fibbonaci sequence of length N in reverse order
-- For example, "fib 5" should return "[3, 2, 1, 1, 0]"
fib :: Int -> [Int]
fib 1 = [0]
fib 2 = [1, 0]
fib n =  let temp=fib(n-1) in (sum $ take 2 temp) : temp

-- Determine whether a given list is a palindrome
-- For example, "palindrome []" or "palindrome [1, 3, 1]" should return "True"
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome (x:[]) = True
palindrome xs = (head(xs) == last(xs)) && (palindrome ( init $ tail xs))

-- Map the first component of a pair with the given function
-- For example, "mapFirst (+3) (4, True)" should return "(7, True)"
mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f pair = ((f  $fst pair ) ,snd pair )

-- Devise a function that has the following type
someFunction :: (a -> b -> c) -> (a -> b) -> a -> c
someFunction f g x = f  x $g(x)
-- ex: someFunction max (3*)  5
