
/**
* my first commitment
* dynamic programming
* memo
*   1. dp definition: dp[i][j] means the best profit we can have at i-th day in state j of without holding / holding / cooldown 
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution1 {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices == null || prices.isEmpty) return 0
        /*
        * state definition
        * 0: without holding
        * 1: holding one
        * 2: sold than cooldown
        */
        val dp = Array.tabulate(prices.length, 3){
            case (0, 0) => 0
            case (0, 1) => -prices(0)
            case (0, 2) => 0
            case _ => 0
        }
        
        for(i <- 1 until prices.length) {
            dp(i)(0) = dp(i - 1)(0) max dp(i - 1)(2) // 0 -> 0 or 2 -> 0
            dp(i)(1) = dp(i - 1)(1) max (dp(i - 1)(0) - prices(i)) // 1 -> 1 or  0 -> 1
            dp(i)(2) = dp(i - 1)(1) + prices(i) // 1 -> 2
        }
        
        dp.last.max
    }
}

/**
* dynamic programming
* memo
*    ok! lets reduce the dp array size without keep all i-th state
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution1-2 {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices == null || prices.isEmpty) return 0
        var withoutHold = 0
        var hold = Int.MinValue
        var coolDown = 0
        
        for(price <- prices) {
            val withoutHold_ = withoutHold  // keep value
            val hold_ = hold // keep value
            withoutHold = withoutHold max coolDown
            hold = hold max (withoutHold_ - price)
            coolDown = hold_ + price  
        }
        withoutHold max coolDown
    }
}

/**
* dynamic programming - function programming
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution1-3 {
    def maxProfit(prices: Array[Int]): Int = {
        val (withoutHold, hold, cooldown) = prices.foldLeft(0, Int.MinValue, 0) {
            case ((withoutHold, hold, cooldown), cost) => 
             (
                 withoutHold max cooldown,
                 hold max (withoutHold - cost),
                 hold + cost      
             )
        }
        withoutHold max cooldown
    }
}