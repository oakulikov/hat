#!/bin/bash

# Script to run Haskell examples from the monad tutorial

if [ $# -eq 0 ]; then
  echo "Usage: ./run-example.sh <example-number>"
  echo "Example: ./run-example.sh 1 (runs 01_IntroToMonads.hs)"
  echo "Available examples:"
  echo "  1 - Introduction to Monads"
  echo "  2 - Maybe and Either Monads"
  echo "  3 - List and IO Monads"
  echo "  4 - State and Reader Monads"
  echo "  5 - Writer and Custom Monads"
  echo "  6 - Monad Transformers"
  exit 1
fi

example_num=$1
padded_num=$(printf "%02d" $example_num)

case $example_num in
  1)
    file="01_IntroToMonads.hs"
    ;;
  2)
    file="02_MaybeAndEitherMonads.hs"
    ;;
  3)
    file="03_ListAndIOMonads.hs"
    ;;
  4)
    file="04_StateAndReaderMonads.hs"
    ;;
  5)
    file="05_WriterAndCustomMonads.hs"
    ;;
  6)
    file="06_MonadTransformers.hs"
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

# Check which example we're running
if [ "$example_num" -eq 1 ]; then
  # First example doesn't require external packages
  runhaskell "$file"
  if [ $? -ne 0 ]; then
    echo "Execution failed."
    
    # Alternative: try with GHCi
    echo "Trying with GHCi..."
    echo "main" | ghci -v0 "$file"
  fi
elif [ "$example_num" -eq 2 ] || [ "$example_num" -eq 3 ]; then
  # Second and third examples require only containers package
  echo "Running with containers package..."
  runhaskell -package containers "$file"
  if [ $? -ne 0 ]; then
    echo "Execution failed."
    
    # Alternative: try with GHCi
    echo "Trying with GHCi..."
    echo "main" | ghci -v0 -package containers "$file"
  fi
elif [ "$example_num" -eq 4 ] || [ "$example_num" -eq 5 ]; then
  # Examples 4 and 5 require mtl package
  echo "Running with package flags (required for examples 4-5)..."
  runhaskell -package mtl -package containers "$file"
  if [ $? -ne 0 ]; then
    echo "Execution failed."
    
    # Alternative: try with GHCi
    echo "Trying with GHCi..."
    echo "main" | ghci -v0 -package mtl -package containers "$file"
  fi
else
  # Example 6 requires transformers package
  echo "Running with package flags (required for example 6)..."
  runhaskell -package mtl -package transformers -package containers "$file"
  if [ $? -ne 0 ]; then
    echo "Execution failed."
    
    # Alternative: try with GHCi
    echo "Trying with GHCi..."
    echo "main" | ghci -v0 -package mtl -package transformers -package containers "$file"
  fi
fi
