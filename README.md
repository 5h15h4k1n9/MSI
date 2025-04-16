[![Build and Test](https://github.com/5h15h4k1n9/MSI/actions/workflows/build-and-test.yml/badge.svg)](https://github.com/5h15h4k1n9/MSI/actions/workflows/build-and-test.yml)

# Mini Solidity Interpreter

This is a mini interpreter for Solidity language written in Haskell.

## Tasks
- [x] Standard types
  - [x] `int`
  - [x] `bool`
  - [x] `string`
  - [x] `mapping`
  - [x] `array`
- [x] Standard operators
- [x] Functions
  - [x] Declaration
  - [x] Recursion
- [x] Access modifiers
  - [x] `public`
  - [x] `private`
  - [ ] ~~`internal`~~
  - [ ] ~~`external`~~
- [x] Variables
  - [x] Declaration
  - [x] Assignment
- [ ] ~~Constants`~~
- [x] `require`
  - Throws error if condition is false and reverts all changes
- [x] Statements
  - [x] `if`
  - [x] `for`
  - [x] `while` 

## Example of contract

```solidity
contract Example {

    int public value = 0;
    function storeValue(int x) public {	
        value = x;
        require(value < 100, "Huge value");
    }

    mapping(int => int) private cache;

    function privateFib(int arg) private returns (int) {
        if (arg == 0 || arg == 1) {
            return 1;
        }
        int current = cache[arg];
        if (current == 0){
            int result = privateFib(arg - 1) + privateFib(arg - 2);
            cache[arg] = result;
            return result;
        }
        else {
            return current;
        }
    }

    function fib(int arg) public returns (int) {
        return privateFib(arg);
    }

    int public constant arrSize = 10;	
    string[arrSize] private arr;

    function storeInArr(int start, int stop, string calldata newValue) public {
        for (int i = start; i < stop; i = i + 1) {
            require(i < arrSize, "Incorrect stop");
            arr[i] = newValue;
        }
    }

    function readFromArr(int index) public returns (string) {
        require(index < arrSize);
        return arr[index];
    }

}
```

## Example of usage

```
~> msi Example.sol
Example.sol successfully uploaded!
Available functions:
    storeValue(int)
    loadValue() returns (int)
    fib(int) returns (int)
    storeInArr(int, int, string)
    readFromArr(int) returns (string)
Available members:
    value
    arrSize

MSI> storeValue(10)
()
MSI> value
10
MSI> storeValue(101)
Reverted with: Huge value
MSI> value
10
MSI> fib(2)
2
MSI> privateFib(2)
Reverted with: access denied
MSI> unknownFunction()
Reverted with: unknown function
MSI> unknownMember = 3
Reverted with: unknown member
MSI> storeInArr(0, 3, "Haskell > OCaml")
()
MSI> readFromArr(0)
Haskell > OCaml
MSI> storeInArr(0, 12, "Haskell < OCaml")
Reverted with: Incorrect stop
MSI> readFromArr(0)
Haskell > OCaml
MSI> exit
Bye-bye!
~>
```

## Best regards

[KarasssDev](https://github.com/KarasssDev) – for the idea of the project and requirements.
