# Изучение функциональных абстракций в Haskell

Этот репозиторий содержит учебные материалы для изучения Haskell, начиная с основ языка и заканчивая продвинутыми функциональными абстракциями, такими как функторы, аппликативные функторы, монады и линзы.

## Структура репозитория

Репозиторий содержит семь основных учебных курсов:

### 1. Основы Haskell (haskell-basics-tutorial/)

Основные концепции языка Haskell, необходимые для понимания функционального программирования.

- [Типы данных и классы типов](haskell-basics-tutorial/01_TypesAndTypeClasses.hs) - Основные типы данных, определение собственных типов, классы типов
- [Синтаксис функций](haskell-basics-tutorial/02_FunctionSyntax.hs) - Различные способы определения функций в Haskell
- [Функции высших порядков](haskell-basics-tutorial/03_HigherOrderFunctions.hs) - Функции, принимающие или возвращающие другие функции
- [Каррирование](haskell-basics-tutorial/04_Currying.hs) - Частичное применение функций и каррирование
- [Сопоставление с образцом](haskell-basics-tutorial/05_PatternMatching.hs) - Использование сопоставления с образцом для разбора структур данных
- [Интервалы и генераторы списков](haskell-basics-tutorial/06_IntervalsAndListGenerators.hs) - Создание и работа с интервалами и генераторами списков
- [Списки и рекурсия](haskell-basics-tutorial/07_ListsAndRecursion.hs) - Работа со списками и рекурсивные функции
- [Свёртка](haskell-basics-tutorial/08_Folding.hs) - Свёртка (folding) и её применение для обработки структур данных
- [Ленивые вычисления](haskell-basics-tutorial/09_LazyEvaluation.hs) - Ленивые вычисления и бесконечные структуры данных
- [Моноиды](haskell-basics-tutorial/10_Monoids.hs) - Моноиды и их применение в функциональном программировании

### 2. Учебное пособие по функторам (functor-tutorial/)

Функторы - это абстракция, которая позволяет применять функции к значениям, находящимся в контексте.

- [Введение в функторы](functor-tutorial/01_IntroToFunctors.hs) - Основные концепции и определение функторов
- [Стандартные функторы](functor-tutorial/02_CommonFunctors.hs) - Обзор стандартных функторов в Haskell
- [Законы функторов](functor-tutorial/03_FunctorLaws.hs) - Законы, которым должны удовлетворять функторы
- [Создание собственных функторов](functor-tutorial/04_CustomFunctors.hs) - Как создавать собственные функторы
- [Практическое применение функторов](functor-tutorial/05_PracticalFunctors.hs) - Реальные примеры использования функторов
- [Функторы и другие абстракции](functor-tutorial/06_FunctorsAndBeyond.hs) - Связь функторов с другими абстракциями

### 3. Учебное пособие по аппликативным функторам (applicative-tutorial/)

Аппликативные функторы - это расширение функторов, которое позволяет применять функции в контексте к значениям в контексте.

- [Введение в аппликативные функторы](applicative-tutorial/01_IntroToApplicatives.hs) - Базовая теория и концепции аппликативных функторов
- [Аппликативный функтор List](applicative-tutorial/02_ListApplicative.hs) - Недетерминированные вычисления с аппликативным функтором List
- [Аппликативный функтор IO](applicative-tutorial/03_IOApplicative.hs) - Работа с вводом-выводом через аппликативный функтор IO
- [Создание собственных аппликативных функторов](applicative-tutorial/04_CustomApplicatives.hs) - Как создавать собственные аппликативные функторы
- [Практическое применение аппликативных функторов](applicative-tutorial/05_PracticalApplicatives.hs) - Решение реальных задач с помощью аппликативных функторов
- [Аппликативные функторы и монады](applicative-tutorial/06_ApplicativesAndMonads.hs) - Связь между аппликативными функторами и монадами

### 4. Учебное пособие по монадам (monad-tutorial/)

Монады - это расширение аппликативных функторов, которое позволяет последовательно комбинировать вычисления в контексте.

- [Введение в монады](monad-tutorial/01_IntroToMonads.hs) - Базовая теория и концепции монад, включая законы монад:
  * Левая идентичность: `return a >>= f` ≡ `f a`
  * Правая идентичность: `m >>= return` ≡ `m`
  * Ассоциативность: `(m >>= f) >>= g` ≡ `m >>= (\x -> f x >>= g)`
- [Монады Maybe и Either](monad-tutorial/02_MaybeAndEitherMonads.hs) - Обработка ошибок с помощью монад Maybe и Either
- [Монады List и IO](monad-tutorial/03_ListAndIOMonads.hs) - Недетерминированные вычисления и ввод-вывод
- [Монады State и Reader](monad-tutorial/04_StateAndReaderMonads.hs) - Работа с состоянием и окружением
- [Монада Writer и создание собственных монад](monad-tutorial/05_WriterAndCustomMonads.hs) - Логирование и создание собственных монад
- [Трансформеры монад](monad-tutorial/06_MonadTransformers.hs) - Комбинирование монад с помощью трансформеров

### 5. Учебное пособие по вводу-выводу (io-tutorial/)

Ввод-вывод в Haskell реализован через монаду IO, которая позволяет выполнять операции с побочными эффектами в чистом функциональном языке.

- [Основы ввода-вывода](io-tutorial/01_BasicIO.hs) - Базовые операции ввода-вывода, монада IO, do-нотация
- [Работа с файлами](io-tutorial/02_FileIO.hs) - Чтение и запись файлов, обработка текстовых данных
- [Работа с директориями](io-tutorial/03_DirectoryIO.hs) - Операции с файловой системой, создание и удаление директорий
- [Бинарный ввод-вывод](io-tutorial/04_BinaryIO.hs) - Работа с бинарными данными, сериализация и десериализация
- [Сетевой ввод-вывод](io-tutorial/05_NetworkIO.hs) - Работа с сетью, HTTP-запросы, сокеты
- [Продвинутый ввод-вывод](io-tutorial/06_AdvancedIO.hs) - Асинхронный ввод-вывод, обработка исключений, ресурсы

### 6. Учебное пособие по линзам (lens-tutorial/)

Линзы - это абстракция, которая позволяет работать с вложенными структурами данных в функциональном стиле.

- [Введение в линзы](lens-tutorial/01_IntroToLenses.hs) - Основные концепции и определение линз
- [Стандартные линзы](lens-tutorial/02_CommonLenses.hs) - Обзор стандартных линз в библиотеке lens
- [Создание собственных линз](lens-tutorial/03_SimpleLenses.hs) - Как создавать собственные линзы
- [Композиция линз](lens-tutorial/04_LensComposition.hs) - Как комбинировать линзы для работы с вложенными структурами
- [Практическое применение линз](lens-tutorial/05_PracticalLenses.hs) - Реальные примеры использования линз
- [Призмы и траверсалы](lens-tutorial/06_PrismsAndTraversals.hs) - Другие оптики, связанные с линзами

### 7. Продвинутый учебник по Haskell (advanced-haskell-tutorial/)

Продвинутые темы языка Haskell для тех, кто уже освоил основы и хочет углубить свои знания.

- [Обобщенные алгебраические типы данных (GADTs)](advanced-haskell-tutorial/01_GADTs.hs) - Расширенные возможности алгебраических типов данных
- [Программирование на уровне типов](advanced-haskell-tutorial/02_TypeLevelProgramming.hs) - Вычисления на уровне типов
- [Зависимые типы](advanced-haskell-tutorial/03_DependentTypes.hs) - Эмуляция зависимых типов в Haskell
- [Продвинутые классы типов](advanced-haskell-tutorial/04_AdvancedTypeClasses.hs) - Многопараметрические классы типов, функциональные зависимости
- [Free монады](advanced-haskell-tutorial/05_FreeMonads.hs) - Построение монад из функторов
- [Расширяемые эффекты](advanced-haskell-tutorial/06_ExtensibleEffects.hs) - Альтернатива трансформерам монад
- [Монадические морфизмы](advanced-haskell-tutorial/07_MonadMorphisms.hs) - Преобразования между монадами
- [Коммонады](advanced-haskell-tutorial/08_Comonads.hs) - Двойственность к монадам
- [Продвинутая рекурсия](advanced-haskell-tutorial/09_AdvancedRecursion.hs) - Схемы рекурсии и катаморфизмы
- [Продвинутые функторы](advanced-haskell-tutorial/10_AdvancedFunctors.hs) - Бифункторы, профункторы и другие вариации
- [Продвинутое использование оптики](advanced-haskell-tutorial/11_AdvancedOptics.hs) - Глубокое погружение в линзы, призмы и траверсалы
- [Оптимизация производительности](advanced-haskell-tutorial/12_PerformanceOptimization.hs) - Техники оптимизации Haskell-программ

## Иерархия типовых классов в Haskell

Функциональное программирование в Haskell основано на нескольких ключевых абстракциях, которые образуют иерархию типовых классов:

1. **Functor** - Позволяет применять функции к значениям в контексте
   - `fmap :: (a -> b) -> f a -> f b`

2. **Applicative** - Расширяет Functor, позволяя применять функции в контексте к значениям в контексте
   - `pure :: a -> f a`
   - `(<*>) :: f (a -> b) -> f a -> f b`

3. **Monad** - Расширяет Applicative, позволяя последовательно комбинировать вычисления в контексте
   - `return :: a -> m a`
   - `(>>=) :: m a -> (a -> m b) -> m b`

4. **Traversable** - Позволяет обходить структуру данных, применяя эффекты в определенном порядке
   - `traverse :: Applicative f => (a -> f b) -> t a -> f (t b)`

5. **Foldable** - Позволяет свертывать структуру данных в одно значение
   - `foldr :: (a -> b -> b) -> b -> t a -> b`

Линзы не являются частью стандартной иерархии типовых классов, но они предоставляют мощный инструмент для работы с вложенными структурами данных.

## Рекомендуемый порядок изучения

1. Начните с изучения основ Haskell, чтобы понять базовые концепции языка
2. Затем переходите к изучению функторов, так как они являются базовой абстракцией
3. После этого изучите аппликативные функторы, которые расширяют функторы
4. Далее изучите монады, которые расширяют аппликативные функторы
5. Изучите ввод-вывод в Haskell, чтобы понять, как работать с внешним миром в чистом функциональном языке
6. Перейдите к линзам, которые предоставляют удобный способ работы с вложенными структурами данных
7. Наконец, когда вы освоите все предыдущие темы, переходите к продвинутым темам Haskell, таким как GADTs, программирование на уровне типов, зависимые типы и другие

## Запуск примеров

Для запуска примеров используйте скрипты `run-example.sh` в каждой директории:

```bash
# Для запуска примера из учебника по основам Haskell
cd haskell-basics-tutorial
./run-example.sh 1  # Запустит пример из 01_TypesAndTypeClasses.hs

# Для запуска примера из учебника по функторам
cd functor-tutorial
./run-example.sh 1  # Запустит пример из 01_IntroToFunctors.hs

# Для запуска примера из учебника по аппликативным функторам
cd applicative-tutorial
./run-example.sh 1  # Запустит пример из 01_IntroToApplicatives.hs

# Для запуска примера из учебника по монадам
cd monad-tutorial
./run-example.sh 1  # Запустит пример из 01_IntroToMonads.hs

# Для запуска примера из учебника по вводу-выводу
cd io-tutorial
./run-example.sh 1  # Запустит пример из 01_BasicIO.hs

# Для запуска примера из учебника по линзам
cd lens-tutorial
./run-example.sh 1  # Запустит пример из 01_IntroToLenses.hs

# Для запуска примера из продвинутого учебника по Haskell
cd advanced-haskell-tutorial
./run-example.sh 1  # Запустит пример из 01_GADTs.hs
```

## Требования

Для запуска примеров вам потребуется:

1. GHC (Glasgow Haskell Compiler)
2. Несколько библиотек Haskell:
   - lens (для примеров с линзами)
   - containers (для Map и Set)
   - text (для работы с Text)
   - vector (для работы с Vector)
   - network (для примеров с сетевым вводом-выводом)
   - bytestring (для примеров с бинарным вводом-выводом)
   - async (для примеров с асинхронным вводом-выводом)
   - directory (для примеров с файловой системой)
   - time (для примеров с логированием и профилированием)

### Установка GHC

```bash
# На Ubuntu/Debian
sudo apt-get install ghc

# На macOS с Homebrew
brew install ghc
```

### Установка библиотек

#### Используя Cabal

```bash
# Создайте новый проект
mkdir -p haskell-tutorial
cd haskell-tutorial
cabal init

# Отредактируйте файл .cabal, добавив зависимости
# build-depends: base, lens, containers, text, vector, network, bytestring, async, directory, time

# Установите зависимости
cabal install --lib lens containers text vector network bytestring async directory time deepseq ghc-prim parallel mtl
```

#### Используя Stack

```bash
# Создайте новый проект
stack new haskell-tutorial
cd haskell-tutorial

# Отредактируйте файл package.yaml, добавив зависимости
# dependencies:
# - base
# - lens
# - containers
# - text
# - vector
# - network
# - bytestring
# - async
# - directory
# - time
# - deepseq
# - ghc-prim
# - parallel
# - mtl

# Установите зависимости
stack install lens containers text vector network bytestring async directory time deepseq ghc-prim parallel mtl
```

### Запуск примеров без установки библиотек

Если вы не хотите устанавливать библиотеки, вы можете запустить примеры с помощью GHCi, указав нужные пакеты:

```bash
# Для примеров с функторами
ghci -XNoImplicitPrelude functor-tutorial/01_IntroToFunctors.hs

# Для примеров с вводом-выводом
ghci -package network -package bytestring -package async -package directory -package time io-tutorial/05_NetworkIO.hs

# Для примеров с линзами
ghci -package lens -package containers -package text -package vector lens-tutorial/01_IntroToLenses.hs

# Для продвинутых примеров
ghci -package deepseq -package time -package ghc-prim -package bytestring -package text -package vector -package parallel -package lens -package mtl -package containers advanced-haskell-tutorial/12_PerformanceOptimization.hs
```

### Примечание о зависимостях

Примеры в этом репозитории разделены на категории:

1. **Примеры с основами Haskell** (haskell-basics-tutorial/) - не требуют дополнительных библиотек, используют только стандартную библиотеку Haskell.
2. **Примеры с функторами** (functor-tutorial/) - не требуют дополнительных библиотек, используют только стандартную библиотеку Haskell.
3. **Примеры с аппликативными функторами** (applicative-tutorial/) - не требуют дополнительных библиотек, используют только стандартную библиотеку Haskell.
4. **Примеры с монадами** (monad-tutorial/) - не требуют дополнительных библиотек, используют только стандартную библиотеку Haskell.
5. **Примеры с вводом-выводом** (io-tutorial/) - некоторые примеры могут требовать дополнительных библиотек, таких как network и bytestring.
6. **Примеры с линзами** (lens-tutorial/) - требуют установки библиотеки lens и других зависимостей.
7. **Продвинутые примеры** (advanced-haskell-tutorial/) - требуют установки различных библиотек, включая deepseq, time, ghc-prim, bytestring, text, vector, parallel, lens, mtl и containers.

Если у вас возникают проблемы с запуском примеров с линзами, вы можете сосредоточиться на примерах с основами Haskell, функторами, аппликативными функторами и монадами, которые не требуют дополнительных библиотек.

## Дополнительные ресурсы

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Haskell Wiki](https://wiki.haskell.org/)
- [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
- [Документация библиотеки lens](https://hackage.haskell.org/package/lens)
