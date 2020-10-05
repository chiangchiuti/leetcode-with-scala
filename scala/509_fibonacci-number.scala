/**
* time complexity O(N)
* space complexity O(1)
*/
object Solution {
    def fib(N: Int): Int = {
        if (N <= 1) N
        else {
            var a = 0
            var b = 1
            (1 until N).foreach{ n =>
                val c = a + b
                a = b
                b = c
            }
            b
        }
    }
}

/**
* recursive version
* time complexity: O(2^N）
*/

object Solution {
    def fib(N: Int): Int = {
        if (N <= 1) N
        else {
           fib(N - 1) + fib(N - 2)
        }
    }
}