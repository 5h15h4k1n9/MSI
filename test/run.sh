#!/usr/bin/env bash

TESTS_DIR="$PWD/test"
PYAML="$TESTS_DIR/pyaml.sh"
PPDIFF="$TESTS_DIR/ppdiff.sh"

TEST_DIRS=$(find "$TESTS_DIR" -mindepth 1 -maxdepth 1 -type d)
stack build

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

OS=$(uname)

ACTUAL="actual.txt"

SUCCESS=0
FAILED=0

if [ "$1" != "" ]; then
    TEST_DIRS=$1
fi

for TEST_GROUP in $TEST_DIRS
do
    if [ ! -d "$TEST_GROUP" ] ; then
        continue
    fi

    CONTRACT_PATH="$TEST_GROUP/contract.sol"
    CONFIG_PATH="$TEST_GROUP/config.yaml"
    TEST_GROUP_NAME=$("$PYAML" "$CONFIG_PATH")

    echo -e "${YELLOW}$TEST_GROUP_NAME tests:${NC}"

    TESTS=$(find "$TEST_GROUP" -mindepth 1 -maxdepth 1 -type d)
    for TEST in $TESTS
    do
        EXPECTED="$TEST/test.expected"
        stack run "$CONTRACT_PATH" < "$TEST/test" &> $ACTUAL

        if [ "$OS" == "Darwin" ]; then
            sed -i '' "s/MSI> //g" $ACTUAL
        elif [ "$OS" == "Linux" ]; then
            sed -i "s/MSI> //g" $ACTUAL
        else
            echo "Unknown OS: $OS"
            exit 1
        fi

        TEST_NAME=$("$PYAML" "$TEST/config.yaml")

        TARGET_LENGTH=100
        TEST_NAME_LENGTH=${#TEST_NAME}
        FILL_LENGTH=$((TARGET_LENGTH - TEST_NAME_LENGTH))
        FILL=$(printf '%*s' "$FILL_LENGTH" '' | tr ' ' '-')

        # shellcheck disable=SC2034
        dif=$(diff "$EXPECTED" $ACTUAL)

        # shellcheck disable=SC2181
        if [ $? -eq 0 ] ; then
          echo -e "   Test ${YELLOW}${TEST_NAME}${NC} ${FILL}$GREEN passed${NC}"
          SUCCESS=$((SUCCESS + 1))
        else
          echo -e "   Test ${YELLOW}${TEST_NAME}${NC} ${FILL}$RED failed${NC}\n"
          "$PPDIFF" "$ACTUAL" "$EXPECTED" --only-diff

          echo -e "\n      Test file:       $TEST/test"
          echo -e "      Expected result: $EXPECTED"
          echo -e "      Contract file:   $CONTRACT_PATH\n"
          rm $ACTUAL

          FAILED=$((FAILED + 1))
        fi
    done
done

echo -e "\n${YELLOW}Summary:${NC}"
echo -e "   ${GREEN}Passed: $SUCCESS${NC}"
echo -e "   ${RED}Failed: $FAILED${NC}"
echo -e "   ${YELLOW}Total:  $((SUCCESS + FAILED))${NC}"

if [ -f "$ACTUAL" ]; then
  rm $ACTUAL
fi

if [ $FAILED -ne 0 ]; then
    exit 1
fi

exit 0