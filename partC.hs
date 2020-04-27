
-- Palindrome Function --
turingPalindrome :: [Char] -> Char
turingPalindrome (xs)   | length (xs) > 4               = qReject                                                       --reject any string with length greater tahn 4
                        | (xs) == ""                    = qAccept                                                       --accept the empty string
                        | inTMAlphabet (xs) == False    = qReject                                                       --reject any strings not from the alphabet {'a','b'}
                        | otherwise                     = q0 (Tape (repeat ' ') (head xs) (tail xs ++ repeat ' '))      --compute the palindrome

-- Tape Declaration --
data Tape a = Tape [a] a [a]

-- Operations to move the Tape --
moveLeft :: Tape a -> Tape a
moveLeft  (Tape (x:xs) y zs) = Tape xs x (y:zs)

moveRight :: Tape a -> Tape a
moveRight (Tape xs y (z:zs)) = Tape (y:xs) z zs

write :: Tape a -> a -> Tape a
write (Tape xs y zs) n = Tape xs n zs

-- State Functions --
q0 (Tape xs y zs)   | y == 'a'  = q1 (moveRight (write (Tape xs y zs) ' '))     --transition to state q1
                    | y == 'b'  = q2 (moveRight (write (Tape xs y zs) ' '))     --transition to state q2
                    | y == ' '  = qAccept                                       --accept the palindrome

q1 (Tape xs y zs)   | y == ' '  = q3 (moveLeft (Tape xs y zs))                  --reached end of tape, move left and transition to q3
                    | otherwise = q1 (moveRight (Tape xs y zs))                 --move right until end of tape

q2 (Tape xs y zs)   | y == ' '  = q4 (moveLeft (Tape xs y zs))                  --reached end of tape, move left and transition to q4
                    | otherwise = q2 (moveRight (Tape xs y zs))                 --move right until end of tape

q3 (Tape xs y zs)   | y == 'b' = qReject                                        --start character not same as end character, reject the palindrome
                    | y == ' ' = qAccept                                        --no characters left, accept the palindrome
                    | y == 'a' = q5 (moveLeft (write (Tape xs y zs) ' '))       --start character the same as end character, transition to q5

q4 (Tape xs y zs)   | y == 'a' = qReject                                        --start character not same as end character, reject the palindrome
                    | y == ' ' = qAccept                                        --no characters left, accept the palindrome
                    | y == 'b' = q5 (moveLeft (write (Tape xs y zs) ' '))       --start character the same as end character, transition to q5

q5 (Tape xs y zs)   | y == ' '  = q0 (moveRight (Tape xs y zs))                 --check if it is a blank (i.e. far left of tape), if so move right and transition to q0.
                    | otherwise = q5 (moveLeft (Tape xs y zs))                  --otherwise, keep moving left

qAccept = 'a'

qReject = 'b'

-- Check to see if a string only contains the characters 'a' and 'b' --
inTMAlphabet :: [Char] -> Bool
inTMAlphabet (xs)   | length xs == 0      = True                    --all symbols processed and in the alphabet {'a','b'}, return True
                    | head xs == 'a'      = inTMAlphabet (tail xs)  --symbol 'a' in the alphabet {'a','b'}, recursive call on remaining elements
                    | head xs == 'b'      = inTMAlphabet (tail xs)  --symbol 'b' in the alphabet {'a','b'}, recursive call on remaining elements
                    | otherwise           = False                   --symbol not in the alphabet {'a','b'}, return False