module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED FUNCTIONS (f1..fN)
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (3 marks)
-- Define 'f1' that takes a list of elements and returns a new list
-- containing every 183rd element of the input list.
-- f1 [1..1000] = [183,366,549,732,915]
f1 :: [a] -> [a]
f1 xs = f1Helper (drop 182 xs)


-- Q2 (3 marks)
-- Define 'f2' that takes a list of integers and returns the sum of every 267th element.
-- f2 [1..1000] = 1602
f2 :: [Int] -> Int
f2 ns = f2Helper (drop 266 ns)


-- Q3 (4 marks)
-- Define 'f3' that takes a list of integers and returns the product of every 343rd element.
-- f3 [1..1000] = 235298
f3 :: [Int] -> Int
f3 ns = f3Helper (drop 342 ns) 1


-- Q4 (8 marks)
-- Define 'f4' that processes a list of 'Maybe Int' values based on an operation table.
-- f4 [Just 42] = (0,[])
f4 mis = processOperations mis 0

-- Define 'processOperations' to process the list of 'Maybe Int' values.
processOperations :: [Maybe Int] -> Int -> (Int, [Maybe Int])
processOperations [] result = (result, [])  -- Base case: return the final result and an empty list.
processOperations (Just opcode : rest) result =
    case opcode of
        -- Handle different opcodes by calling appropriate functions.
        57 -> processFixedOperation rest 6 result add
        90 -> processFixedOperation rest 6 result add
        78 -> processFixedOperation rest 4 result (addWithReplacement 8)
        82 -> processStopOperation rest 4 result add
        51 -> processStopOperation rest 4 result add
        97 -> processStopOperation rest 4 result (addWithReplacement 0)
        83 -> processFixedOperation rest 5 result mul
        30 -> processFixedOperation rest 4 result mul
        31 -> processFixedOperation rest 4 result (mulWithReplacement 4)
        71 -> processStopOperation rest 4 result mul
        76 -> processStopOperation rest 5 result mul
        86 -> processStopOperation rest 5 result (mulWithReplacement 6)
        _ -> processOperations rest result
processOperations (Nothing : rest) result = processOperations rest result

-- Define 'processFixedOperation' to handle fixed-length operations.
processFixedOperation :: [Maybe Int] -> Int -> Int -> (Int -> Int -> Int) -> (Int, [Maybe Int])
processFixedOperation ops n result operation
    | length ops >= n =
        let (operands, remaining) = splitAt n ops
            processed = foldl (\acc x -> maybe 0 id x) 1 operands
            newResult = operation result processed
        in processOperations remaining newResult
    | otherwise = (result, ops)

-- Define 'processStopOperation' to handle stop-length operations.
processStopOperation :: [Maybe Int] -> Int -> Int -> (Int -> Int -> Int) -> (Int, [Maybe Int])
processStopOperation ops stop result operation
    | length ops >= stop =
        let (operands, remaining) = splitAt stop ops
            processed = foldl (\acc x -> maybe 0 id x) 1 operands
            newResult = operation result processed
        in (newResult, remaining)
    | otherwise = (result, ops)

-- Define 'addWithReplacement' to handle replacement during addition.
addWithReplacement :: Int -> Int -> Int -> Int
addWithReplacement result operand replacement
    | operand == 8 = replacement
    | otherwise = result + operand

-- Define 'mulWithReplacement' to handle replacement during multiplication.
mulWithReplacement :: Int -> Int -> Int -> Int
mulWithReplacement result operand replacement
    | operand == 4 = replacement
    | otherwise = result * operand


-- Q5 (2 marks)
-- Define 'f5' that uses 'f4' to process all opcodes in the maybe list.
-- f5 [Just 42] = [0]
f5 mis = processOpcodes mis []

-- Define 'processOpcodes' to repeatedly apply 'f4' and accumulate results.
processOpcodes :: [Maybe Int] -> [Int] -> [Int]
processOpcodes [] results = reverse results  -- Reverse the results list to maintain order.
processOpcodes mis results =
    let (result, remaining) = f4 mis
    in processOpcodes remaining (result : results)




-- Add extra material below here, such as helper functions, test values, etc.



-- Define a helper function 'f1Helper' that extracts every 183rd element
-- starting from the 182nd element of a list.
f1Helper :: [a] -> [a]
f1Helper [] = []  -- Base case: an empty list results in an empty list.
f1Helper (x:xs) = x : f1Helper (drop 182 xs)


-- Define a helper function 'f2Helper' to sum every 267th element
-- starting from the 266th element of a list.
f2Helper :: [Int] -> Int
f2Helper [] = 0  -- Base case: an empty list results in 0.
f2Helper (x:xs) = x + f2Helper (drop 266 xs)


-- Define a helper function 'f3Helper' to multiply every 343rd element
-- starting from the 342nd element of a list, with an initial accumulator of 1.
f3Helper :: [Int] -> Int -> Int
f3Helper [] result = result  -- Base case: return the accumulated result.
f3Helper (x:xs) result = f3Helper (drop 342 xs) (result * x)