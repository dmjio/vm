vm
=================

A tiny virtual machine.

### example
```haskell
module Main where

import VM

main :: IO ()
main = dumpRegisters (execute program)

program :: VM ()
program = do
  loadi r0 (imm 100)
  loadi r1 (imm 200)
  add r2 r0 r1
  halt
```

### result
```bash
registers: 64 c8 12c
```

### build
```bash
nix-build
```

### develop
```bash
nix-shell
```
