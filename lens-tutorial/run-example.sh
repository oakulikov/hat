#!/bin/bash

# Script to run Haskell examples from the lens tutorial

if [ $# -eq 0 ]; then
  echo "Usage: ./run-example.sh <example-number>"
  echo "Example: ./run-example.sh 1 (runs 01_IntroToLenses.hs)"
  echo "Available examples:"
  echo "  1 - Introduction to Lenses"
  echo "  2 - Common Lenses"
  echo "  3 - Custom Lenses"
  echo "  4 - Lens Composition"
  echo "  5 - Practical Lenses"
  echo "  6 - Prisms and Traversals"
  exit 1
fi

example_num=$1
padded_num=$(printf "%02d" $example_num)

case $example_num in
  1)
    file="01_IntroToLenses.hs"
    ;;
  2)
    file="02_CommonLenses.hs"
    ;;
  3)
    file="03_SimpleLenses.hs"
    ;;
  4)
    file="04_LensComposition.hs"
    ;;
  5)
    file="05_PracticalLenses.hs"
    ;;
  6)
    file="06_PrismsAndTraversals.hs"
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
else
  # For other examples, include all necessary packages
  echo "Running with package flags (required for examples 4-6)..."
  runhaskell -package containers "$file"
  if [ $? -ne 0 ]; then
    echo "Execution failed."
    
    # Alternative: try with GHCi
    echo "Trying with GHCi..."
    echo "main" | ghci -v0 -package containers "$file"
  fi
fi
