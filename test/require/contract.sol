contract RequireProperties {
    int public counter = 0;
    bool public flag = false;

    function simpleCheck(int x) public {
        require(x > 0, "x must be positive");
        counter = x;
    }

    function withMessage(bool condition) public {
        require(condition, "Condition failed");
        flag = true;
    }

    function nestedRequire(int x) public {
        require(x < 100, "Too big");
        require(x > 10, "Too small");
        counter = x;
    }

    function multiEffect(int a, int b) public {
        require(a < b, "Invalid order");
        counter = b - a;
        flag = true;
    }

    function earlyReturn(int x) public {
        if (x != 0) {
            require(x < 50, "Too big");
            counter = x;
        }
    }
}
