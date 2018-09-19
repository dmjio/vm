module Main where

import VM

program :: VM ()
program = do
  loadi r0 (imm 100)
  loadi r1 (imm 200)
  add r2 r0 r1
  halt

main :: IO ()
main = dumpRegisters (execute program)


