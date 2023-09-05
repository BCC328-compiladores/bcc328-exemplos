module Stack.Semantics.Interp ( Code
                              , Value
                              , Address
                              , VMState (..)
                              , VMError (..)
                              , runVM
                              )where

import Control.Monad.Except
import Control.Monad.State

import Data.Bits
import Data.Char
import qualified Data.Map as Map

import Stack.Syntax.Instr


-- definition of the machine state

type Code = [Instr]
type Value = Int
type Address = Int

data VMState
  = VMState {
       pc :: Int
    ,  code :: [Instr]
    ,  memory :: Map.Map Address Value
    ,  stack :: [Value]
    } deriving Show

-- initial machine state

initialState :: Code  -> VMState
initialState c = VMState 0 c Map.empty []

-- definition of the monad

data VMError
  = InvalidStackSize
  | InvalidPC
  | InvalidInput

type VM a = StateT VMState (ExceptT VMError IO) a

runVM :: Code -> IO (Either VMError VMState)
runVM prog 
  = do
       res <- runExceptT (runStateT cpu (initialState prog))
       return $ either Left (Right . snd) res

-- getting the next instruction

fetchInstr :: VM Instr
fetchInstr
  = do
       curr <- gets pc
       prog <- gets code
       let codeSize = length prog
       if curr > codeSize - 1 then throwError InvalidPC
         else return (prog !! curr)

-- machine cycle

cpu :: VM ()
cpu = do
          next <- fetchInstr
          case next of
            HALT -> return ()
            _    -> do
                      execInstr next
                      if isJump next then return ()
                      else incPC
                      cpu

isJump :: Instr -> Bool
isJump (JMP _) = True
isJump (JZ _) = True
isJump (JNZ _) = True
isJump _ = False

execInstr :: Instr -> VM ()
execInstr NOOP = noop
execInstr (PUSHI n) = pushImmediate n
execInstr (PUSH n) = pushAddress n
execInstr (POP n) = popAddress n
execInstr (JMP n) = jump n
execInstr (JZ n) = jumpIfZero n
execInstr (JNZ n) = jumpIfNotZero n
execInstr PUSHPC = pushPC
execInstr POPPC = popPC
execInstr IN = input
execInstr OUT = output
execInstr ADD = add
execInstr SUB = sub
execInstr MUL = mul
execInstr SHL = shiftLeft
execInstr SHR = shiftRight
execInstr BAND = bitwiseAnd
execInstr BOR = bitwiseOr
execInstr BXOR = bitwiseXor
execInstr AND = booleanAnd
execInstr OR = booleanOr
execInstr IEQ = eq
execInstr NE = neq
execInstr GE = geq
execInstr LE = leq
execInstr IGT = gt
execInstr ILT = lt
execInstr NEG = neg
execInstr BNOT = bitwiseNot
execInstr NOT = booleanNot
execInstr LOAD = load
execInstr STORE = store
execInstr HALT = return ()



-- instruction semantics

noop :: VM ()
noop = return ()

pushImmediate :: Value -> VM ()
pushImmediate n
  = modify (\ s -> s {stack = n : stack s})

pushAddress :: Address -> VM ()
pushAddress addr
  = do
       v <- readMemory addr
       pushImmediate v

popAddress :: Value -> VM ()
popAddress val
  = do
       top <- pop
       storeMemory top val

jump :: Int -> VM ()
jump = changePC

jumpIfZero :: Int -> VM ()
jumpIfZero newPC
  = do
      val <- pop
      if val == 0 then changePC newPC
        else return ()

jumpIfNotZero :: Int -> VM ()
jumpIfNotZero newPC
  = do
      val <- pop
      if val /= 0 then changePC newPC
        else return ()

load :: VM ()
load = do
         addr <- pop
         val <- readMemory addr
         pushImmediate val

store :: VM ()
store = do
          top <- pop
          next <- pop
          storeMemory next top

pushPC :: VM ()
pushPC = currentPC >>= pushImmediate

popPC :: VM ()
popPC = pop >>= changePC

input :: VM ()
input = do
          s <- liftIO getLine
          if all isDigit s then pushImmediate (read s)
            else throwError InvalidInput
output :: VM ()
output = do
           val <- pop
           liftIO $ print val

add :: VM ()
add = binaryOp (+)

sub :: VM ()
sub = binaryOp (-)

mul :: VM ()
mul = binaryOp (*)

shiftLeft :: VM ()
shiftLeft = binaryOp shiftL

shiftRight :: VM ()
shiftRight = binaryOp shiftR

bitwiseAnd :: VM ()
bitwiseAnd = binaryOp (.&.)

bitwiseOr :: VM ()
bitwiseOr = binaryOp (.|.)

bitwiseXor :: VM ()
bitwiseXor = binaryOp xor

booleanAnd :: VM ()
booleanAnd = binaryOp f
  where
    f x y = castBool (castInt x && castInt y)

booleanOr :: VM ()
booleanOr = binaryOp f
  where
    f x y = castBool (castInt x || castInt y)

eq :: VM ()
eq = binaryOp equality
  where
    equality x y = castBool (x == y)

neq :: VM ()
neq = binaryOp f
  where
    f x y = castBool (x /= y)

geq :: VM ()
geq = binaryOp f
  where
    f x y = castBool (x >= y)

leq :: VM ()
leq = binaryOp f
  where
    f x y = castBool (x <= y)

gt :: VM ()
gt = binaryOp f
  where
    f x y = castBool (x > y)

lt :: VM ()
lt = binaryOp f
  where
    f x y = castBool (x < y)


neg :: VM ()
neg = do
        val <- pop
        pushImmediate (negate val)

bitwiseNot :: VM ()
bitwiseNot
  = do
        val <- pop
        pushImmediate (complement val)

booleanNot :: VM ()
booleanNot
  = do
       val <- pop
       pushImmediate (castBool (not (castInt val)))

-- auxiliar functions to manipulate state

binaryOp :: (Int -> Int -> Int) -> VM ()
binaryOp f
  = do
      top <- pop
      next <- pop
      pushImmediate (f next top)

currentPC :: VM Int
currentPC = gets pc

changePC :: Int -> VM ()
changePC newPC
  = modify (\ s -> s {pc = newPC})

incPC :: VM ()
incPC
  = do
      currPc <- currentPC
      changePC (currPc + 1)

storeMemory :: Address -> Value -> VM ()
storeMemory addr val
  = modify (\ s -> s {memory = Map.insert addr val (memory s)})

readMemory :: Address -> VM Value
readMemory addr
  = do
      v <- gets (Map.lookup addr . memory)
      return $ maybe 0 id v

pop :: VM Value
pop = do
        st <- gets stack
        case st of
          [] -> throwError InvalidStackSize
          (v : _) -> return v

castBool :: Bool -> Int
castBool True = 1
castBool _ = 0

castInt :: Int -> Bool
castInt 0 = False
castInt _ = True
