contract WhileLogic {

    function countUpTo(int limit) public returns (int) {
        int i = 0;
        while (i < limit) {
            i = i + 1;
        }
        return i;
    }

    function sumTo(int n) public returns (int) {
        int i = 1;
        int sum = 0;
        while (i <= n) {
            sum = sum + i;
            i = i + 1;
        }
        return sum;
    }

    function doubleUntil(int x) public returns (int) {
        int value = 1;
        while (value < x) {
            value = value * 2;
        }
        return value;
    }

    function infiniteSafe(int maxSteps) public returns (int) {
        int i = 0;
        while (true) {
            i = i + 1;
            if (i == maxSteps) {
                break;
            }
        }
        return i;
    }

    function countNested(int outerLimit, int innerLimit) public returns (int) {
        int total = 0;
        int i = 0;
        while (i < outerLimit) {
            int j = 0;
            while (j < innerLimit) {
                total = total + 1;
                j = j + 1;
            }
            i = i + 1;
        }
        return total;
    }

    function breakInner(int outerLimit, int innerLimit, int breakAt) public returns (int) {
        int total = 0;
        int i = 0;
        while (i < outerLimit) {
            int j = 0;
            while (j < innerLimit) {
                if (j == breakAt) {
                    break;
                }
                total = total + 1;
                j = j + 1;
            }
            i = i + 1;
        }
        return total;
    }

    function triangleCount(int n) public returns (int) {
        int total = 0;
        int i = 1;
        while (i <= n) {
            int j = 1;
            while (j <= i) {
                total = total + 1;
                j = j + 1;
            }
            i = i + 1;
        }
        return total;
    }
}
