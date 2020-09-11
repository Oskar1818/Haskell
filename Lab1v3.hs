{- Lab 1
   Authors: Clara Josefsson, Oskar Sturebrand, Valter Miari
   Lab group: 59 -}
---------------------------------------------
import MeasureTime
import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)


-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k
   | k >= 0 = k + 1
   | k < 0 = error "only uses non negative integers"


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k = product [n | x <- [1..k]]


-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
   | k < 0 = error "power: negative argument"
   | k == 0 = 1
   | even k = power2 (n*n) (div k 2)
   | odd k = n * power2 n (k-1)


-- D -------------------------
{-
<Describe your test cases here>
- error case : k < 0, ex. function 4 -1,
Why this case doesn't generate an error in the test function,
instead returns True; is because in the functions comapare1 and 2,
all the input integers are converted to their absolute value,
hence no negative values, hence no errors generated.

- case1 : k = 0, ex. function 3 0
This test case doesn't execute recursively rather linearly, because it satisfies
a condition before the recursion in the function excecutes.

- case2 : k = n > 0, ex. function 6 5
A standard case which excecutes recursively.
 ...
 -}


-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k


-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k


-- Test functions
test :: Bool
test = and [comparePower1 n k && comparePower2 n k | (n,k) <- testcases]
  where testcases = [(3,0),(6,5),(4,(-1)), (50, 0), (23, 6), (78, (-2))]


{- MeasureTime generated the following results:
measureTime2 power 10 100000
Start evaluation
Done after 2.9s
...
measureTime2 power1 10 100000
Start evaluation
Done after 2.8s
...
measureTime2 power2 10 100000
Start evaluation
Done after 0.017s
-}
