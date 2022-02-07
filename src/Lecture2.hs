{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight
    , createKnight
    , Dragon
    , createDragon
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct [] = 1
lazyProduct (0:_) = 0
lazyProduct (x:xs) = x * lazyProduct xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt:: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 0 (x:xs) = (Just x, xs)
removeAt n xs
  | n < 0 = (Nothing, xs)
  | otherwise = bisect n [] xs
    where
      bisect _ _ [] = (Nothing, xs)
      bisect 0 left (y:ys) = (Just y, left <> ys)
      bisect position left (y:ys) = bisect (position - 1) (left <> [y]) ys

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists:: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces:: [Char] -> [Char]
dropSpaces = head . words

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
  * As a reward, knight takes all the gold, the treasure and dExp.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 dExp points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strikes
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strikes decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you bisect the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)
newtype Health = Health Int
newtype Attack = Attack Int
newtype Endurance = Endurance Int
newtype StrikesCount = StrikesCount Int
newtype Gold = Gold Int
  deriving Show
newtype Experience = Experience Int
  deriving Show

data Knight = Knight
  { kHealth    :: Health
  , kAttack    :: Attack
  , kEndurance :: Endurance
  , kStrikes   :: StrikesCount
  }
data Treasure = Armor | Weapon | Gems
  deriving Show
data Chest = Chest Gold (Maybe [Treasure])
data DragonType = Red | Black | Green
data Dragon = Dragon
  { dExperience :: Experience
  , dChest :: Chest
  , dHealth :: Health
  , dAttack :: Attack
  }
data FightOutcome = Victory Experience Gold (Maybe [Treasure]) | Death | Escape
  deriving Show

createKnight:: Health -> Attack -> Endurance -> Knight
createKnight health attack endurance = Knight health attack endurance (StrikesCount 1)

createDragon:: DragonType -> Gold -> [Treasure] -> Health -> Attack -> Dragon
createDragon Red gold treasure health attack = Dragon
  { dExperience = Experience 100
  , dChest = Chest gold (Just treasure)
  , dHealth = health
  , dAttack = attack
  }
createDragon Black gold treasure health attack = Dragon
  { dExperience = Experience 150
  , dChest = Chest gold (Just treasure)
  , dHealth = health
  , dAttack = attack
  }
createDragon Green gold _ health attack = Dragon
  { dExperience = Experience 250
  , dChest = Chest gold Nothing
  , dHealth = health
  , dAttack = attack
  }

incrementKnightStrike:: Knight -> Knight
incrementKnightStrike knight@(Knight _ _ _ (StrikesCount count)) =
  knight { kStrikes = StrikesCount (count + 1) }

resetKnightStrike:: Knight -> Knight
resetKnightStrike knight = knight { kStrikes = StrikesCount 0 }

hit:: Health -> Attack -> Health
hit (Health health) (Attack damage) = Health (health - damage)

hitDragon:: Knight -> Dragon -> Dragon
hitDragon knight dragon = dragon { dHealth = hit (dHealth dragon) (kAttack knight)}

hitKnight:: Dragon -> Knight -> Knight
hitKnight dragon knight = knight { kHealth = hit (kHealth knight) (dAttack dragon)}

dragonFight:: Knight -> Dragon -> FightOutcome
dragonFight
  knight@(Knight (Health knightHealth) _ (Endurance endurance) (StrikesCount strikes))
  dragon@(Dragon dExp (Chest gold treasures) (Health dragonHealth) _)
    | knightHealth <= 0 = Death
    | endurance <= 0 = Escape
    | strikes == 10 = dragonFight (resetKnightStrike $ hitKnight dragon knight) dragon
    | dragonHealth <= 0 = Victory dExp gold treasures
    | otherwise = dragonFight (incrementKnightStrike knight) (hitDragon knight dragon)

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered more challenging. However,
you still may bisect some of them easier than some of the previous
ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:y:ys) = (x <= y) && isIncreasing (y:ys)

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y : merge (x:xs) ys
  | x < y = x : merge xs (y:ys)
  | otherwise = x : y : merge xs ys

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit x) = Right x
eval [] (Var varName) = Left (VariableNotFound varName)
eval vars (Var varName) = maybeToEither $ lookup varName vars
  where
    maybeToEither:: Maybe Int -> Either EvalError Int
    maybeToEither Nothing = Left (VariableNotFound varName)
    maybeToEither (Just y) = Right y
eval vars (Add expr1 expr2) = sumEither (eval vars expr1) (eval vars expr2)
  where
    sumEither:: Either EvalError Int -> Either EvalError Int -> Either EvalError Int
    sumEither (Left (VariableNotFound x)) _ = Left (VariableNotFound x)
    sumEither _ (Left (VariableNotFound x)) = Left (VariableNotFound x)
    sumEither (Right x) (Right y) = Right (x + y)

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
constantFolding :: Expr -> Expr
constantFolding expr = case expr of
  (Lit x) -> Lit x
  (Var x) -> Var x

  (Add (Lit x) (Lit y)) -> Lit (x + y)
  (Add (Lit 0) x) -> x
  (Add x (Lit 0)) -> x

  (Add (Var x) (Add y z)) -> Add (Var x) $ constantFolding $ Add y z
  (Add (Add x y) (Var z)) -> Add (Var z) $ constantFolding $ Add x y
  (Add (Lit x) (Add (Lit y) z)) -> constantFolding $ Add z $ Lit $ x + y
  (Add (Lit x) (Add y (Lit z))) -> constantFolding $ Add y $ Lit $ x + z
  (Add (Add (Lit x) y) (Lit z)) -> constantFolding $ Add y $ Lit $ x + z
  (Add (Add x (Lit y)) (Lit z)) -> constantFolding $ Add x $ Lit $ y + z

  (Add (Add w x) (Add y z)) -> constantFolding $ Add w $ Add x $ Add y z
  
  _ -> expr