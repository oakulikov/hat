#!/bin/bash

# Скрипт для запуска примеров из учебника по продвинутому Haskell

if [ $# -eq 0 ]; then
  echo "Использование: ./run-example.sh <номер-примера>"
  echo "Пример: ./run-example.sh 1 (запускает 01_GADTs.hs)"
  echo "Доступные примеры:"
  echo "  1 - Обобщенные алгебраические типы данных (GADTs)"
  echo "  2 - Программирование на уровне типов"
  echo "  3 - Зависимые типы"
  echo "  4 - Продвинутые классы типов"
  echo "  5 - Free монады"
  echo "  6 - Расширяемые эффекты"
  echo "  7 - Монадические морфизмы"
  echo "  8 - Коммонады"
  echo "  9 - Продвинутая рекурсия"
  echo "  10 - Продвинутые функторы"
  echo "  11 - Продвинутое использование оптики"
  echo "  12 - Оптимизация производительности"
  exit 1
fi

example_num=$1
padded_num=$(printf "%02d" $example_num)

case $example_num in
  1)
    file="01_GADTs.hs"
    ghc_opts=""
    ;;
  2)
    file="02_TypeLevelProgramming.hs"
    ghc_opts=""
    ;;
  3)
    file="03_DependentTypes.hs"
    ghc_opts=""
    ;;
  4)
    file="04_AdvancedTypeClasses.hs"
    ghc_opts=""
    ;;
  5)
    file="05_FreeMonads.hs"
    ghc_opts=""
    ;;
  6)
    file="06_ExtensibleEffects.hs"
    ghc_opts=""
    ;;
  7)
    file="07_MonadMorphisms.hs"
    ghc_opts=""
    ;;
  8)
    file="08_Comonads.hs"
    ghc_opts=""
    ;;
  9)
    file="09_AdvancedRecursion.hs"
    ghc_opts=""
    ;;
  10)
    file="10_AdvancedFunctors.hs"
    ghc_opts=""
    ;;
  11)
    file="11_AdvancedOptics.hs"
    ghc_opts=""
    ;;
  12)
    file="12_PerformanceOptimization.hs"
    ghc_opts="-package deepseq -package time -package ghc-prim -package bytestring -package text -package vector -package parallel"
    ;;
  *)
    echo "Неверный номер примера. Пожалуйста, выберите число от 1 до 12."
    exit 1
    ;;
esac

if [ ! -f "$file" ]; then
  echo "Ошибка: Файл $file не найден."
  exit 1
fi

echo "Запуск примера $example_num: $file"
echo "-----------------------------------"

# Запуск примера с помощью runhaskell (без компиляции)
runhaskell $ghc_opts "$file"
if [ $? -ne 0 ]; then
  echo "Выполнение не удалось."
  
  # Альтернатива: попробовать с GHCi
  echo "Пробуем с GHCi..."
  echo "main" | ghci -v0 $ghc_opts "$file"
fi
