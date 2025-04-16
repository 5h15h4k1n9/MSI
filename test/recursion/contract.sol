contract Recursion {
    int public val = 0;

    function factorial(int n) public returns (int) {
        if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }

    function fibonacci(int n) public returns (int) {
        if (n <= 1) {
            return n;
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    }

    function even(int n) public returns (int) {
        if (n == 0) {
            return 1;
        } else {
            return odd(n - 1);
        }
    }

    function odd(int n) public returns (int) {
        if (n == 0) {
            return 0;
        } else {
            return even(n - 1);
        }
    }

    function ack(int m, int n) public returns (int) {
        if (m == 0) {
            return n + 1;
        } else if (m > 0 && n == 0) {
            return ack(m - 1, 1);
        } else if (m > 0 && n > 0) {
            return ack(m - 1, ack(m, n - 1));
        }
        return 0; // This line is unreachable but added to satisfy the compiler
    }

    function fastPow(int base, int exp) public returns (int) {
        if (exp == 0) {
            return 1;
        } else if (exp % 2 == 0) {
            int half = fastPow(base, exp / 2);
            return half * half;
        } else {
            return base * fastPow(base, exp - 1);
        }
    }
}