module Main where

import Control.Monad.State
import Data.Char
import Text.ParserCombinators.Parsec
import System.Environment
import qualified Data.Sequence as Seq

data BrainfuckInstruction = 
    IncPtr          |
    DecPtr          |
    IncByte         |
    DecByte         |
    OutByte         |
    AcceptByte      |
    LoopStart       |
    LoopCheck       deriving (Eq)

instance Show BrainfuckInstruction where
    show IncPtr     = ">"
    show DecPtr     = "<"
    show IncByte    = "+" 
    show DecByte    = "-"
    show OutByte    = "."
    show AcceptByte = ","
    show LoopStart  = "["
    show LoopCheck  = "]"

showBrainfuckProgram :: [BrainfuckInstruction] -> String
showBrainfuckProgram ls = concatMap show ls

toCommand IncPtr     = next
toCommand DecPtr     = prev
toCommand IncByte    = plus
toCommand DecByte    = minus
toCommand OutByte    = putC
toCommand AcceptByte = getC
toCommand LoopStart  = loopStart
toCommand LoopCheck  = loopEnd

{- 
Parser
-}
brainfuck :: Parser BrainfuckInstruction
brainfuck =
    do
        char '>'
        return IncPtr
    <|>
    do
        char '<'
        return DecPtr
    <|>
    do
        char '+'
        return IncByte
    <|>
    do
        char '-'
        return DecByte
    <|>
    do
        char '.'
        return OutByte
    <|>
    do
        char ','
        return AcceptByte
    <|>
    do
        char '['
        return LoopStart
    <|>
    do
        char ']'
        return LoopCheck

brainfuckProgram :: Parser [BrainfuckInstruction]
brainfuckProgram = 
    do
        bf <- sepBy (many brainfuck) (noneOf "><+-.,[]")
        return (concat bf) 

parseBrainfuck input = parse brainfuckProgram "brainfuck" input

parseBrainfuckFile fp = 
    do
        fileStr <- readFile fp
        return $ parseBrainfuck fileStr

{-
Interpreter
-}
data BrainfuckContext = BrainfuckContext {instructionPointer :: Int,dataPointer :: Int, array :: Seq.Seq Int, loopStack :: [Int]}

type Bf = StateT BrainfuckContext IO ()

instance Show BrainfuckContext where
    show (BrainfuckContext i p arr ls) = show arr

arr :: [Int]
arr = take 100 $ repeat 0

initContext = BrainfuckContext 0 0 (Seq.fromList arr) []

next :: Bf
next = do
    BrainfuckContext i p arr ls <- get
    put $ BrainfuckContext (i+1) (p+1) arr ls
    return ()

prev :: Bf
prev = do
    BrainfuckContext i p arr ls <- get
    put $ BrainfuckContext (i+1) (p-1) arr ls
    return ()

plus :: Bf
plus = do
    BrainfuckContext i p arr ls <- get
    put $ BrainfuckContext (i+1) p (Seq.update p ((Seq.index arr p)+1) arr) ls
    return ()

minus :: Bf
minus = do
    BrainfuckContext i p arr ls <- get
    put $ BrainfuckContext (i+1) p (Seq.update p ((Seq.index arr p)-1) arr) ls
    return ()

putC :: Bf
putC = do
    BrainfuckContext i p arr ls <- get
    let c = chr $ Seq.index arr p
    liftIO $ putChar c
    put $ BrainfuckContext (i+1) p arr ls
    return ()

getC :: Bf
getC = do
    BrainfuckContext i p arr ls <- get
    c <- liftIO $ getChar
    put $ BrainfuckContext (i+1) p (Seq.update p (ord c) arr) ls
    return ()

loopStart :: Bf
loopStart = do
    BrainfuckContext i p arr ls <- get
    put $ BrainfuckContext (i+1) p arr (push i ls)

loopEnd :: Bf
loopEnd = do
    BrainfuckContext i p arr ls <- get
    let pointer = (Seq.index arr p)
    if pointer == 0 
        then do
            let (_,lsNext) = pop ls
            put $ BrainfuckContext (i+1) p arr lsNext
            next 
        else do
            let (nxtIns,_) = pop ls 
            put $ BrainfuckContext nxtIns p arr ls
            return ()

evalInstructions :: Int -> [BrainfuckInstruction] -> Bf
evalInstructions _ []   = return ()
evalInstructions n xs   = do
    if n >= (length xs)
        then
            return ()
        else do
            toCommand (xs !! n)
            BrainfuckContext i p arr ls <- get
            evalInstructions i xs            
    
push :: Int -> [Int] -> [Int]
push a ls = a:ls

pop :: [Int] -> (Int,[Int])
pop (x:xs) = (x,xs)

main :: IO ()
main = do
    args <- getArgs
    if (length args) > 0 
        then do
            let filePath = head args
            parseResult <- parseBrainfuckFile filePath
            case parseResult of
                -- Error
                Left a -> print a
                -- Everything is good.
                Right a -> do
                    putStrLn (showBrainfuckProgram a)
                    (_,foo) <- runStateT (evalInstructions 0 a) initContext
                    print foo
                    return ()
        else do
            print "The first argument should be the path to a brainfuck file."