#!/bin/bash

# Script to run Haskell examples from the applicative functor tutorial

if [ $# -eq 0 ]; then
  echo "Usage: ./run-example.sh <example-number>"
  echo "Example: ./run-example.sh 1 (runs 01_IntroToApplicatives.hs)"
  echo "Available examples:"
  echo "  1 - Introduction to Applicative Functors"
  echo "  2 - List Applicative"
  echo "  3 - IO Applicative"
  echo "  4 - Custom Applicatives"
  echo "  5 - Practical Applicatives"
  echo "  6 - Applicatives and Monads"
  exit 1
fi

example_num=$1
padded_num=$(printf "%02d" $example_num)

case $example_num in
  1)
    file="01_IntroToApplicatives.hs"
    ;;
  2)
    file="02_ListApplicative.hs"
    ;;
  3)
    file="03_IOApplicative.hs"
    ;;
  4)
    file="04_CustomApplicatives.hs"
    ;;
  5)
    file="05_PracticalApplicatives.hs"
    ;;
  6)
    file="06_ApplicativesAndMonads.hs"
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
