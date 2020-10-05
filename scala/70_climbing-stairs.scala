/**
* dynamic programming
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution {
    def climbStairs(n: Int): Int = {
        if(n <= 2) n
        else {
            val cache = Array.ofDim[Int](n + 1)
            cache(0) = 1
            cache(1) = 1
            (2 to n).foreach{ nn =>
                cache(nn) = cache(nn - 1) + cache(nn - 2)
            }
            cache(n)
        }
    }
}



/**
* only use two extra space to keep previous two value
* time complexity: O(N)
* space complexity: O(1)
*/

object Solution2 {
    def climbStairs(n: Int): Int = {
        if(n <= 2) n
        else {
            var a = 1
            var b = 2
            (3 to n).foreach{ nn =>
                val c = a + b
                a = b
                b = c    
            }
            b
        }
    }
}


object Solution3 {
    def climbStairs(n: Int): Int = {
        var a = 0
        var b = 1
        for (_ <- 0 until n) {
            val c = a + b
            a = b
            b = c
        }
        b
    }
}