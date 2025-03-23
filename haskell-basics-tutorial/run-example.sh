#!/bin/bash

# Проверка наличия аргумента
if [ $# -ne 1 ]; then
  echo "Использование: $0 <номер_примера>"
  echo "Например: $0 1 (запустит пример из 01_TypesAndTypeClasses.hs)"
  exit 1
fi

# Получение номера примера
EXAMPLE_NUM=$1

# Формирование имени файла
case $EXAMPLE_NUM in
  1) FILE="01_TypesAndTypeClasses.hs" ;;
  2) FILE="02_FunctionSyntax.hs" ;;
  3) FILE="03_HigherOrderFunctions.hs" ;;
  4) FILE="04_Currying.hs" ;;
  5) FILE="05_PatternMatching.hs" ;;
  6) FILE="06_IntervalsAndListGenerators.hs" ;;
  7) FILE="07_ListsAndRecursion.hs" ;;
  8) FILE="08_Folding.hs" ;;
  9) FILE="09_LazyEvaluation.hs" ;;
  10) FILE="10_Monoids.hs" ;;
  *) echo "Неверный номер примера: $EXAMPLE_NUM"; exit 1 ;;
esac

echo "Running example $EXAMPLE_NUM: $FILE"
echo "-----------------------------------"

# Запуск примера
runhaskell $FILE

# Если runhaskell не сработал, пробуем запустить через GHCi
if [ $? -ne 0 ]; then
  echo "Execution failed."
  echo "Trying with GHCi..."
  ghci $FILE
fi
