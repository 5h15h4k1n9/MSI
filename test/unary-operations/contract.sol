contract UnaryOps {
    int public number = 0;
    bool public flag = false;

    function negate(int x) public returns (int) {
        return -x;
    }

    function logicalNot(bool x) public returns (bool) {
        return !x;
    }
}
