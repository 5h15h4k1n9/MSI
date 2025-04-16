contract AccessControl {
    int private secret = 42;
    int public value = 0;

    function getSecret() private returns (int) {
        return secret;
    }

    function revealSecret() public returns (int) {
        return getSecret();
    }

    function setValue(int newValue) public {
        value = newValue;
    }

    function secretDouble() private returns (int) {
        return secret * 2;
    }

    function getDoubleViaPublic() public returns (int) {
        return secretDouble();
    }
}
