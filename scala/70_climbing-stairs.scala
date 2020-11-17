/**
* select solution
* dynamic programming
* memo
*   1. dp(i) represent climb to i floor's distinct ways
*   2. dp(i) could be calculate from dp(i - 1) + dp(i - 2)
*           (1) taking a single step from dp(i - 1)
*           (2) taking a step of two from dp(i - 2)
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution0 {
    def climbStairs(n: Int): Int = {
        val dp = Array.ofDim[Int](n + 1)
        dp(0) = 1
        dp(1) = 1
        (2 to n).foreach(i => dp(i) = dp(i - 1) + dp(i - 2))
        dp(n)
    }
}

/**
* my first commitment
* dynamic programming
* memo:
*   1. dp(i) represent climb to i floor's distinct ways
*   2. dp(i) could be calculate from dp(i - 1) + dp(i - 2)
*           (1) taking a single step from dp(i - 1)
*           (2) taking a step of two from dp(i - 2)
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution1 {
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
*  simplify from 1
*/
object Solution1-2 {
    def climbStairs(n: Int): Int = {
        val dp = Array.ofDim[Int](n + 1)
        dp(0) = 1
        dp(1) = 1
        (2 to n).foreach(i => dp(i) = dp(i - 1) + dp(i - 2))
        dp(n)
    }
}



/**
* DP: only use two extra space to keep previous two value
* time complexity: O(N)
* space complexity: O(1)
*/

object Solution1-3 {
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

/**
* dp: index from 0 until n
*   it would be confusing with index i original meaning which is the ways of climbing to stair i
* memo:
*  1. keep two previous status
*/
object Solution1-4 {
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