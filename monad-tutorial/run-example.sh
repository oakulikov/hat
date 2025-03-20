#!/bin/bash

# Script to run Haskell examples from the monad tutorial

if [ $# -eq 0 ]; then
  echo "Usage: ./run-example.sh <example-number>"
  echo "Example: ./run-example.sh 1 (runs 01_IntroToMonads.hs)"
  echo "Available examples:"
  echo "  1 - Introduction to Monads"
  echo "  2 - List Monad"
  echo "  3 - IO Monad"
  echo "  4 - Custom Monads"
  echo "  5 - Monad Transformers"
  echo "  6 - Practical Monads"
  exit 1
fi

example_num=$1
padded_num=$(printf "%02d" $example_num)

case $example_num in
  1)
    file="01_IntroToMonads.hs"
    ;;
  2)
    file="02_ListMonad.hs"
    ;;
  3)
    file="03_IOMonad.hs"
    ;;
  4)
    file="04_CustomMonads.hs"
    ;;
  5)
    file="05_MonadTransformers.hs"
    ;;
  6)
    file="06_PracticalMonads.hs"
    ;;
  *)
    echo "Invalid example number. Please choose a number between 1 and 6."
    exit 1
    ;;
esac

if [ ! -f "$file" ]; then
  echo "Error: File $file not found."
  exit 1
fi

echo "Running example $example_num: $file"
echo "-----------------------------------"

# Run the example using runhaskell (no compilation needed)
runhaskell "$file"
if [ $? -ne 0 ]; then
  echo "Execution failed."
  
  # Alternative: try with GHCi
  echo "Trying with GHCi..."
  echo "main" | ghci -v0 "$file"
fi
