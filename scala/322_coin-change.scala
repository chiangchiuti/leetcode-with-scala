/**
* dynamic programming: bottom up
* time complexity: O(S * N), S is the amount, N is the coin denomination count
* space complexity: O(S)
*/

object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
         
        val dp = Array.fill[Int](amount + 1)(amount + 1) // record the minimum needed coins of each denominations

        dp(0) = 0
        for (i <- 1 to amount; denominations <- coins) {

            if(denominations <= i) {
                dp(i) = dp(i) min (dp(i - denominations) + 1)
            }        
        }
    
        if (dp.last > amount) -1 else dp.last
    }
}
