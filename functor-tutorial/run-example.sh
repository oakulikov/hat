#!/bin/bash

# Script to run Haskell examples from the functor tutorial

if [ $# -eq 0 ]; then
  echo "Usage: ./run-example.sh <example-number>"
  echo "Example: ./run-example.sh 1 (runs 01_IntroToFunctors.hs)"
  echo "Available examples:"
  echo "  1 - Introduction to Functors"
  echo "  2 - Common Functors"
  echo "  3 - Functor Laws"
  echo "  4 - Custom Functors"
  echo "  5 - Practical Functors"
  echo "  6 - Functors and Beyond"
  exit 1
fi

example_num=$1
padded_num=$(printf "%02d" $example_num)

case $example_num in
  1)
    file="01_IntroToFunctors.hs"
    ;;
  2)
    file="02_CommonFunctors.hs"
    ;;
  3)
    file="03_FunctorLaws.hs"
    ;;
  4)
    file="04_CustomFunctors.hs"
    ;;
  5)
    file="05_PracticalFunctors.hs"
    ;;
  6)
    file="06_FunctorsAndBeyond.hs"
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
