import Standard.Pipes hiding (range, list, right)

-- TODO:
-- * Add a "function" thing into state and mutate it whenever offset changes
-- * This is done so that we don't re-make the function each time...

--------------------------------------------------------------------------------
-- Re-implement Standard Library Functions

-- Integer Division (can't use `div`)
(//) = toOperator(quotient)

-- Integer Remainder
(%) = toOperator(remainder)

-- Having issues with "fromOperator"
multiply :: (Number, Number) -> Number
multiply(a, b) = a * b

-- Splits a given list into two lists
-- Slightly scews right on odd numbers, as seen in 2nd case
split :: [a] -> ([a], [a])
split([]) = ([], [])
split([a]) = ([], [a])
split(list) = (left, right)
 where
  halfLength = length(list) // 2
  left = first(list, halfLength)
  right = rest(list, halfLength)

-- Adds seperator element between each element in list
intersperse :: (a, [a]) -> [a]
intersperse(_, []) = []
intersperse(_, [x]) = [x]
intersperse(seperator, x:xs) = x : seperator : intersperse(seperator, xs)

-- Modified implementation of "unwords"
-- This doesn't put a space between each element.
unwordsNoSpace :: [Text] -> Text
unwordsNoSpace([]) = ""
unwordsNoSpace([x]) = x
unwordsNoSpace(x:xs) = x <> unwordsNoSpace(xs)

-- Gets the number of digits
-- Takes a list of Numbers as `Text -> Number` transformation doesn't really work
numberOfDigits :: [Number] -> Number
numberOfDigits(list) = list
                       |> map(\num -> printed(num))
                       |> unwordsNoSpace 
                       |> numberOfCharacters


-- Concatenate list of digits to one number
-- Digit range should be 0..9
concatDigits :: [Number] -> Number
concatDigits([]) = 0
concatDigits(x:xs) = x * 10^numberOfDigits(xs) + concatDigits(xs)

-- Reads two lists and applies the function using both lists
zipWith :: (((a, b) -> c), [a], [b]) -> [c]
zipWith(_, [], _) = []
zipWith(_, _, []) = []
zipWith(f, x:xs, y:ys) = f(x, y) : zipWith(f, xs, ys)

applyToBoth :: ((a -> b), (a, a)) -> (b, b)
applyToBoth(f, (l, r)) = (f(l), f(r))

-- Control.Arrow
(***) = toOperator(applyToBoth)

--------------------------------------------------------------------------------

{- Index is a:
     * List of Strings
-}
type Expression = [Text]

combineExpressions :: (Expression, Expression) -> (Text, Text) 
combineExpressions(l, r) = (left, right)
 where
  left  = intersperse(" + ", l) |> unwordsNoSpace 
  right = intersperse(" + ", r) |> unwordsNoSpace

createExpressions :: (Parameters, [Text]) -> Expression
createExpressions(parameters, functions) = zipWith(_transform, parameters, functions)
                                           |> filter(\text' -> text' /= "")
 where
    _transform :: (Number, Text) -> Text
    _transform(0, str) = ""
    _transform(-1, str) = "-" <> str
    _transform(1, str) = str
    _transform(_, str) = str

  
--------------------------------------------------------------------------------

{- RandomState contains
    * current simulation time
    * list of Random Numbers (0..3^18)
    * current offset for the random number
    * function that takes time and transforms it using the seed
-}
data ProgramState = ProgramState { time :: Time
                                 , random :: [Double]
                                 , randomIndex :: Index
                                 , inputSeed :: InputSeed 
                                 , pointTransformer :: PointTransformer}

initial :: [Number] -> ProgramState
initial(x) = ProgramState { time = -1
                          , random = x
                          , randomIndex = 1
                          , inputSeed = []
                          , pointTransformer = (x # 1)
                                               |> composeSeed 
                                               |> mapSeedToParameters 
                                               |> generatePointTransformer}


--------------------------------------------------------------------------------

{- Double is a:
     * Number (Technically a double/float)
     * Ranges from 0..1
-}
type Double = Number

mapDoubleToSeed :: (Double, Number) -> Seed
mapDoubleToSeed(number, range) = rounded(number * range)

mapSeedToDouble :: (Seed, Number) -> Double
mapSeedToDouble(number, range) = number / range

-- Returns double at randomOffset
getRandomDouble :: ProgramState -> Double
getRandomDouble(state) = random(state) # randomIndex(state)

-- Sets double at randomOffset
setRandomDouble :: (ProgramState, Double) -> ProgramState
setRandomDouble(state, double) = state { random = random' }
 where
  random' = at(random(state), double, randomIndex(state)) 

--------------------------------------------------------------------------------

{- Seed is a:
     * Number
     * Ranges from 0..3^18
-}
type Seed = Number

composeSeed :: Double -> Seed
composeSeed(double) = mapDoubleToSeed(double, 3^18)

decomposeSeed :: Seed -> Double
decomposeSeed(seed) = mapSeedToDouble(seed, 3^18)
  
-- Returns seed at randomOffset
getRandomSeed :: ProgramState -> Seed
getRandomSeed(state) = state 
                       |> getRandomDouble 
                       |> composeSeed

-- Sets seed randomOffset
setRandomSeed :: (ProgramState, Seed) -> ProgramState
setRandomSeed(state, seed) = seed
                             |> decomposeSeed
                             |> \s -> setRandomDouble(state, s)

--------------------------------------------------------------------------------

{- Index is a:
     * Number
-}
type Time = Number

-- Gets current time
getTime :: ProgramState -> Time
getTime(state) = time(state)

-- Sets current time to value
setTime :: (ProgramState, Time) -> ProgramState
setTime(state, time') = state { time = time' }

-- Increments time by offset/deltaTime
incrementTime :: (ProgramState, Time) -> ProgramState
incrementTime(state, deltaTime) = (state, time(state) + deltaTime)
                                  |> setTime

--------------------------------------------------------------------------------

{- Index is a:
     * Number
     * Value should range from 1..inf
     * SHOULD NEVER BE 0
-}
type Index = Number

-- Gets the current offset
getRandomIndex :: ProgramState -> Index
getRandomIndex(state) = randomIndex(state)

-- Sets the current offset
setRandomIndex :: (ProgramState, Index) -> ProgramState
setRandomIndex(state, 0) = error("Indexes start at 1")
setRandomIndex(state, offset') = state { randomIndex = offset' }

-- Increments the current offset
-- Has some extra bounds checking to make sure it's always >0
incrementRandomIndex :: (ProgramState, Index) -> ProgramState
incrementRandomIndex(state, deltaOffset) = state { randomIndex = offset' }
 where
  offset = randomIndex(state)
  offset' = if (offset + deltaOffset > 0) 
            then (offset + deltaOffset)
            else 1

--------------------------------------------------------------------------------

{- InputSeed is a:
     * List of Numbers that can be collapsed
     * The collapsed value should range from 0..3^18
-}
type InputSeed = [Number]

-- Returns a cleared InputSeed and it's value
flushInputSeed :: ProgramState -> (ProgramState, InputSeed)
flushInputSeed(state) = (state { inputSeed = [] }, inputSeed(state))

-- Sets the InputSeed field to empty
clearInputSeed :: ProgramState -> ProgramState
clearInputSeed(state) = flushInputSeed(state)
                        |> firstOfPair

getInputSeed :: ProgramState -> InputSeed
getInputSeed(state) = flushInputSeed(state)
                      |> secondOfPair

-- Appends number to the InputSeed list
appendInputSeed :: (ProgramState, Number) -> ProgramState
appendInputSeed(state, number) = state { inputSeed = append(number, inputSeed(state))  }

-- Collapses InputSeed to Seed
mapInputSeedToSeed :: InputSeed -> Seed
mapInputSeedToSeed(inputSeed) = concatDigits(inputSeed)

--------------------------------------------------------------------------------

{- Parameters is a:
     * List length 18 (always)
     * The only unique values are `-1`, `0`, and `1`.
-}
type Parameters = [Number]

-- Takes a Seed and turns it into Parameters
mapSeedToParameters :: Seed -> Parameters
mapSeedToParameters(seed) = [0..17]
                            |> map(\x -> (seed // (3 ^ x) % 3 - 1))

--------------------------------------------------------------------------------

type PointTransformer = PointTime -> Point

getPointTransformer :: ProgramState -> PointTransformer
getPointTransformer(state) = pointTransformer(state)

updatePointTransformer :: ProgramState -> ProgramState
updatePointTransformer(state) = state
                                |> getRandomSeed
                                |> mapSeedToParameters 
                                |> generatePointTransformer
                                |> \fn -> state { pointTransformer = fn }

generatePointTransformer :: Parameters -> PointTransformer
generatePointTransformer([p1, p2, p3, p4, p5, p6, p7, p8, p9, 
                          p10, p11, p12, p13, p14, p15, p16, p17, p18]) = 
  \((x, y), t) ->
  let
    x' = (p1 * x * x) + (p2 * y * y) 
                      + (p3 * t * t) 
                      + (p4 * x * y) 
                      + (p5 * x * t) 
                      + (p6 * y * t) 
                      + (p7 * x) 
                      + (p8 * y) 
                      + (p9 * t)
    y' = (p10 * x * x) + (p11 * y * y) 
                       + (p12 * t * t) 
                       + (p13 * x * y) 
                       + (p14 * x * t) 
                       + (p15 * y * t) 
                       + (p16 * x) 
                       + (p17 * y) 
                       + (p18 * t)
  in
    (x', y')

--------------------------------------------------------------------------------

type PointTime = (Point, Number)

iterateTransformPoint :: (Number, Number, (PointTime -> Point)) -> [Point]
iterateTransformPoint(time, numPoints, transform) = _privateIterate((time, time), 1)
  where
    _privateIterate :: PointTime -> [Point]
    _privateIterate((x, y), step) 
     | (step == numPoints) = [transform((x, y), time)]
     | (abs (x) > 10000000000000 || abs (y) > 10000000000000) = []
     | (otherwise) = p' : _privateIterate(p', step + 1)
                     where
                       p' = transform((x, y), time)

-- Hard-coded the functions and array indexes to make the code "more efficient"
-- Takes the parameters and returns a function which operates over points

makeFunctionList :: ([Number]) -> ([Text], [Text])
makeFunctionList(parameters) = (createExpressions(left, functions),
                                createExpressions(right, functions))
  where
    functions = ["x²", "y²", "t²", "xy", "xt", "yt", "x", "y", "t"]
    
    (left, right) = split(parameters)
            
--------------------------------------------------------------------------------

    
program :: Effect
program = activityOf(initial, change, picture)

-- Gonna have to do some hacky state-management in which I hijack state to build a fake prompt menu by intercepting number buttons and appending it to a list in state and on enter convert it to a number and set random at the current index to it (with "at()")

change :: (ProgramState, Event) -> ProgramState

change(s, KeyPress("Right")) = incrementRandomIndex(s, 1) 
                               |> updatePointTransformer
                               |> \s' -> setTime(s', -1)
change(s, KeyPress("Left")) = incrementRandomIndex(s, -1) 
                              |> updatePointTransformer
                              |> \s' -> setTime(s', -1)

change(s, KeyPress("Up")) = incrementTime(s, 0.25)
change(s, KeyPress("Down")) = incrementTime(s, -0.25)

change(s, KeyPress("1")) = appendInputSeed(s, 1)
change(s, KeyPress("2")) = appendInputSeed(s, 2)
change(s, KeyPress("3")) = appendInputSeed(s, 3)
change(s, KeyPress("4")) = appendInputSeed(s, 4)
change(s, KeyPress("5")) = appendInputSeed(s, 5)
change(s, KeyPress("6")) = appendInputSeed(s, 6)
change(s, KeyPress("7")) = appendInputSeed(s, 7)
change(s, KeyPress("8")) = appendInputSeed(s, 8)
change(s, KeyPress("9")) = appendInputSeed(s, 9)
change(s, KeyPress("0")) = appendInputSeed(s, 0)

change(s, KeyPress("Backspace")) = clearInputSeed(s)
change(s, KeyPress("Enter")) = flushInputSeed(s)
                               |> \(state, inputSeed) -> (state, mapInputSeedToSeed(inputSeed))
                               |> setRandomSeed
                               |> updatePointTransformer
                               |> \s' -> setTime(s', -1)

change(s, TimePassing(dt)) = incrementTime(s, dt/50)
change(s, _) = s

picture :: ProgramState -> Picture
picture(s) = combined([drawPoints(s), drawTime(s), drawSeed(s), drawFunction(s), drawInputSeed(s)])

drawTime :: ProgramState -> Picture
drawTime(s) = getTime(s)
              |> printed
              |> (\text -> lettering("Time: " <> text))
              |> (\text -> scaled(text, 0.5, 0.5))
              |> (\text -> translated(text, (-8, -9.5)))

drawInputSeed :: ProgramState -> Picture
drawInputSeed(s) = inputSeed(s) 
                   |> concatDigits 
                   |> printed 
                   |> (\text -> if text == "0" then "" else text) 
                   |> lettering

drawFunction :: ProgramState -> Picture
drawFunction(s) = getRandomSeed(s) 
                  |> mapSeedToParameters 
                  |> makeFunctionList 
                  |> combineExpressions
                  |> \tuple -> lettering *** tuple
                  |> \(x', y') -> [x', translated(y', (0, -2))]
                  |> combined
                  |> (\text -> scaled(text, 0.5, 0.5))
                  |> (\text -> translated(text, (0, 9)))

drawSeed :: ProgramState -> Picture
drawSeed(s) = getRandomSeed(s) 
              |> printed 
              |> (\seed -> lettering("Seed: " <> seed))
              |> (\text -> scaled(text, 0.5, 0.5))
              |> (\text -> translated(text, (0, -9.5)))

drawPoints :: ProgramState -> Picture
drawPoints(s) = combined(dots)
 where
  time = getTime(s)
  numPoints = 2500
  transform = getPointTransformer(s)
  points = iterateTransformPoint(time, numPoints, transform)
  dots = foreach(points, \(x, y) -> dot(x * 4, y * 4))