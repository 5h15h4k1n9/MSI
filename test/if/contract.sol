contract IfExamples {
    function isPositive(int x) public returns (string) {
        if (x > 0) {
            return "positive";
        } else if (x == 0) {
            return "zero";
        } else {
            return "negative";
        }
    }

    function max(int a, int b) public returns (int) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    function nestedCondition(int x) public returns (string) {
        if (x < 10) {
            if (x < 5) {
                return "small";
            } else {
                return "medium";
            }
        }
        return "large";
    }

    function onlyIf(int x) public returns (string) {
        if (x > 0) {
            return "positive";
        }
        return "not positive";
    }
}
