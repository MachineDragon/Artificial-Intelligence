 solveShuffle :: [Char] -> [Char] -> [[Char]] 
-- method 1
 solveShuffle (x : xs) (y : ys) = if x `elem` ys && y `elem` xs && length xs == length ys then aStar curr goal h'Shuffle genStatesShuffle else "[]"                           
 --method 2 
 solveShuffle curr goal = if aStar curr goal h'Shuffle genStatesShuffle /= [] then anotherSolution = aStar curr goal h'Shuffle genStatesShuffle else "[]"


-- Bonus
-- An easy way to solve this is to take each individual
-- characters in a word as well as the total number of characters
-- that are present in both words your comparing, if they
-- both have the same amount of characters in the string
-- and each char compares identically while iterating throughout
-- the string to another char of the solution string then
-- it can determine if there is a possible solution before
-- it tries each possible path that could consume a lot of time

-- aStar :: [String] -> [String] -> Bool  #inputs two strings to compare
-- aStar [] = True
-- aStar (x : xs) (y : ys) = x == y && xs == ys && length x == length y else False
--                          
-- iterates through both strings for similar characters and if there
-- is a diffrent char not in both strings returns false



-- Problem 2.a
h'Maze :: (Int, Int) -> (Int, Int) -> Int

h'Maze curr goal = maximum [length $ takeWhile (/= curr!!c) (reverse $ take (c + 1) goal)
 | c <- [0..(length curr) - 1]]



genStatesMaze :: [[Char]] -> (Int, Int) -> [(Int, Int)]
genStatesMaze maze (row, col) = [(row,col) | row <- [row - 1..row + 1] , col <- [col - 2 .. col + 2], row /= "#", col /= "#"]




-- Problem 2.b

-- The giant maze can be solved very efficiently despite
-- having hundreds of diffrent states, it does this by using the
-- h' function to determine the more optimistic and
-- efficient path to take to reach the goal state
-- then uses the genStates function to determine
-- possible paths to take from the current position
-- Using the A* algorithm to get through the optimal 
-- path despite having soo many diffrent positions
-- following only the shortest path

-- The A* search tree differs in structure between the mazes
-- and shuffle problems because in the shuffle problem,
-- we are using [Char], and it gets the optimal solution
-- to get from word to word such as  “DIRTYROOM” to “DORMITORY”.
-- however in the maze problem we are using integers instead
-- of chars to get the coordinates of the row and column
-- and moving from row and column to another row and column
-- through the shortest optimal path to get to the exit 
-- They both are similar in finding the optimalist most efficient
-- solution however both are still used for diffrent types 
-- of inputs




--problem 2.c

--  assuming a reasonable h'Maze function, the A* algorithim
--  would only be needed for use once as this algorithm determines
--  the most optimal and shortest path to the exit. However,
--  to get more iterations throughout the A* algorithm then
--  you would need a larger maze, since a larger maze contains more
--  spots and paths, then the A* algorithm would have to iterate through more
--  spots and pathways then it would for a smaller maze, this is how the
--  size would change and require more use of the iterations
--  of the A* algorithm as possible



-- Question 3.a

-- propositions 

-- A: If the course is late or boring Hans will fall asleep
-- B: Whenever Hans falls asleep he will start snoring
-- C: Hans does not snore 
-- D: It is not late therefore the course is not boring

-- Hypothesis

-- A ∨ B
-- C ∨ D

-- Conclusion
-- (A ∧ C) ∨ (B ∧ D)

-- The argument is invalid



-- Question 3.b

-- proposition

-- A:Anne goes to the library on Saturday
-- B:Anne goes to the library on Sunday
-- C:John goes to the library on Saturday
-- D: John goes to the library on Sunday
-- TEST: Anne and John will be at the library on Saturday
-- or BOTH on Sunday


-- Hypothesis

-- A ∨ B
-- C ∨ D

-- (A ∧ C) ∨ (B ∧ D)

-- Conclusion


-- The argument is invalid
-- since Anne or John are either there on Saturday
-- Or Sunday, there is a chance that Anne and John both go 
-- on seperate days therefore negating the argument as 
-- invalid



-- Question 3.c

--proposition

-- A: if we could travel back in time
-- B: we can visit ouselves in the past
-- C: If somone had visited themselves in the past they would have publicized it
-- D: No one has ever publicized it

-- Hypothesis

--(C ∧ D) ∧ (A ∧ B)

-- Conclusion

-- Since D is true, no one has publicized the even therefore
-- negating A ∧ B

-- The argument is invalid



-- Question 3.d

-- Proposition

-- A: Carl drinks a bottle of whiskey
-- B: Carl gets terrible headaches
-- C: Carl drinks a bottle of rum
-- D: Carl believes he sees pink elephants after drinking rum
-- E: Carl drinks a bottle of water
-- F: Carl feels bored after drinking water
-- G: Car drinks one or more bottles
-- H: Carl does not believe he sees pink elephants
-- I: Carl is not bored

-- Hypothesis
-- (A ∧ B) ∨ (C ∧ D) ∨  (E ∧ F)
-- (G ∧ H ∧ I)

-- Conclusion
-- A
-- since Carl is not bored, does not see pink elephants
-- this negates C and E therefore leaving only whiskey as
-- the drink he chose

-- The argument is valid











