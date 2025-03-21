#!/bin/bash

# Проверка наличия аргумента
if [ $# -ne 1 ]; then
    echo "Использование: $0 <номер_примера>"
    echo "Пример: $0 1"
    exit 1
fi

# Получение номера примера
EXAMPLE_NUM=$1
EXAMPLE_FILE=$(printf "%02d_IO" $EXAMPLE_NUM)

if [ $EXAMPLE_NUM -eq 1 ]; then
    EXAMPLE_FILE="01_BasicIO"
elif [ $EXAMPLE_NUM -eq 2 ]; then
    EXAMPLE_FILE="02_FileIO"
elif [ $EXAMPLE_NUM -eq 3 ]; then
    EXAMPLE_FILE="03_DirectoryIO"
elif [ $EXAMPLE_NUM -eq 4 ]; then
    EXAMPLE_FILE="04_BinaryIO"
elif [ $EXAMPLE_NUM -eq 5 ]; then
    EXAMPLE_FILE="05_NetworkIO"
elif [ $EXAMPLE_NUM -eq 6 ]; then
    EXAMPLE_FILE="06_AdvancedIO"
fi

EXAMPLE_FILE="${EXAMPLE_FILE}.hs"

echo "Running example $EXAMPLE_NUM: $EXAMPLE_FILE"
echo "-----------------------------------"

# Определение пакетов в зависимости от примера
PACKAGES="directory filepath time temporary"

if [ $EXAMPLE_NUM -eq 4 ]; then
    PACKAGES="$PACKAGES binary bytestring"
elif [ $EXAMPLE_NUM -eq 5 ]; then
    PACKAGES="$PACKAGES bytestring http-client http-conduit http-types aeson text network"
elif [ $EXAMPLE_NUM -eq 6 ]; then
    PACKAGES="$PACKAGES bytestring async stm random process"
fi

# Формирование строки с пакетами
PACKAGES_STR=""
for pkg in $PACKAGES; do
    PACKAGES_STR="$PACKAGES_STR -package $pkg"
done

# Запуск примера с помощью runhaskell с необходимыми пакетами
runhaskell $PACKAGES_STR $EXAMPLE_FILE

# Если runhaskell завершился с ошибкой, попробуем запустить через GHCi
if [ $? -ne 0 ]; then
    echo "Execution failed."
    echo "Trying with GHCi..."
    # Формирование команды для установки пакетов в GHCi
    GHCI_PACKAGES=":set $PACKAGES_STR"
    
    # Используем echo для передачи команд в GHCi через стандартный ввод
    (echo "$GHCI_PACKAGES"; 
     echo ":load $EXAMPLE_FILE"; 
     echo "main"; 
     echo ":quit") | ghci
fi
