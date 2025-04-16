<a href="https://github.com/5h15h4k1n9/MSI/actions"><img alt="Build Status" src="https://github.com/5h15h4k1n9/MSI/actions/workflows/build.yml/badge.svg"></a>
<a href="https://github.com/5h15h4k1n9/MSI/actions"><img alt="Test Status" src="https://github.com/5h15h4k1n9/MSI/actions/workflows/test.yml/badge.svg"></a>

# Mini Solidity Interpreter

This is a mini interpreter for Solidity language written in Haskell.

## Tasks
- [ ] Standard types
  - [ ] `uint`
  - [ ] `bool`
  - [ ] `string`
  - [ ] `mapping`
  - [ ] `array`
- [ ] Standard operators
- [ ] Functions
  - [ ] Declaration
  - [ ] Recursion
- [ ] Access modifiers
  - [ ] `public`
  - [ ] `private`
  - [ ] ~~`internal`~~
  - [ ] ~~`external`~~
- [ ] Variables
  - [ ] Declaration
  - [ ] Assignment
- [ ] Constants
- [ ] `require`
  - Throws error if condition is false and reverts all changes
- [ ] Statements
  - [ ] `if`
  - [ ] `for`

## Example of contract

```solidity
contract Example {

    uint public value = 0;
    function storeValue(uint x) public {	
        value = x;
        require(value < 100, "Huge value");
    }

    mapping(uint => uint) private cache;
    function privateFib(uint arg) private returns (uint) {
        if (arg == 0 || arg == 1) {
            return 1;
        }
        uint current = cache[arg];
        if (current == 0){
            uint result = privateFib(arg - 1) + privateFib(arg - 2);
            cache[arg] = result;
            return result;
        }
        else {
            return current;
        }
    }

    function fib(uint arg) public returns (uint) {
        return privateFib(arg);
    }

    uint public constant arrSize = 10;	
    string[arrSize] private arr;

    function storeInArr(uint start, uint stop, string calldata newValue) public {
        for (uint i = start; i < stop; i++) {
            require(i < arrSize, "Incorrect stop");
            arr[i] = newValue;
        }
    }

    function readFromArr(uint index) public returns (string memory) {
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
    storeValue(uint)
    loadValue() returns (uint)
    fib(uint) returns (uint)
    storeInArr(uint, uint, string)
    readFromArr(uint) returns (string)
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