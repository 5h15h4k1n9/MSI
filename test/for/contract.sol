contract ForLogic {
    function factorial(int n) public returns (int) {
        int result = 1;
        for (int i = 1; i <= n; i = i + 1) {
            result = result * i;
        }
        return result;
    }

    function sumEven(int limit) public returns (int) {
        int sum = 0;
        for (int i = 0; i <= limit; i = i + 1) {
            if (i % 2 == 0) {
                sum = sum + i;
            }
        }
        return sum;
    }

    function countTo(int n) public returns (int) {
        int count = 0;
        for (int i = 0; i < n; i = i + 1) {
            count = count + 1;
        }
        return count;
    }

    function nestedSum(int outer, int inner) public returns (int) {
        int total = 0;
        for (int i = 0; i < outer; i = i + 1) {
            for (int j = 0; j < inner; j = j + 1) {
                total = total + 1;
            }
        }
        return total;
    }

    function breakAt(int limit) public returns (int) {
        int counter = 0;
        for (int i = 0; i < 10; i = i + 1) {
            if (i == limit) {
                break;
            }
            counter = counter + 1;
        }
        return counter;
    }

    function continueOddSum() public returns (int) {
        int sum = 0;
        for (int i = 0; i < 5; i = i + 1) {
            if (i % 2 == 0) {
                continue;
            }
            sum = sum + i;
        }
        return sum;
    }
}
