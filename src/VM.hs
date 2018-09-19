{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VM where

import Control.Monad.State (State, execState)
import Lens.Micro.Platform
import Numeric
import Text.Printf

-- | Machine
data Machine
  = Machine
  { _r0 :: Int
  , _r1 :: Int
  , _r2 :: Int
  , _pc :: Int
  , _state :: Status
  } deriving (Show, Eq)

-- | Status
data Status
  = Running
  | Halted
  deriving (Show, Eq)

makeLenses 'Machine

-- | Immediate (scalar) values begin with the hash mark #, like #100, #200.
newtype Imm = Imm Int
  deriving (Show, Eq)

-- | Immediate smart
imm :: Int -> Imm
imm = Imm

-- | Memory addresses begin with the at sign @, like @1000, @1004.
newtype Addr = Addr Int
  deriving (Show, Eq)

-- | Address
addr :: Int -> Addr
addr = Addr

-- | Register numbers begin with the letter r, like r0, r1, r2.
newtype Reg = Reg Int
  deriving (Show, Eq)

-- | Register
reg :: Int -> Reg
reg = Reg

emptyMachine :: Machine
emptyMachine = Machine 0 0 0 0 Running

-- | Virtual Machine
type VM a = State Machine a

-- | Register lens
type Reg' = Lens' Machine Int

dumpRegisters :: Machine -> IO ()
dumpRegisters (Machine r0 r1 r2 _ _) =
 putStrLn $ printf "registers: %s %s %s"
   (showHex r0 "") (showHex r1 "") (showHex r2 "")

loadi
  :: Reg'
  -> Imm
  -> VM ()
loadi reg (Imm imm)
  = reg .= imm

add
  :: Reg'
  -> Reg'
  -> Reg'
  -> VM ()
add result r1' r2' = do
  r1 <- use r1'
  r2 <- use r2'
  result .= r1 + r2

halt :: VM ()
halt = state .= Halted

execute :: State Machine () -> Machine
execute = do flip execState (emptyMachine & state .~ Running)
