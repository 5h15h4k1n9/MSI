contract VariableOperations {
    int public count = 0;

    function increment() public {
        count = count + 1;
    }

    function decrement() public {
        count = count - 1;
    }

    function reset() public {
        count = 0;
    }

    function add(int value) public {
        count = count + value;
    }

    function requireNonNegative() public {
        require(count >= 0, "Negative count not allowed");
    }
}
