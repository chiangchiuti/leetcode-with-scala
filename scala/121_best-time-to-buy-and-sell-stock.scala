/**
* dynamic programming
* time complexity : O(N)
* space complexity: O(3N)
*/
object Solution1 {
    def maxProfit(prices: Array[Int]): Int = {
        if (prices == null || prices.isEmpty) return 0
        /* 
        * state: 0: without holding, 
        *        1: holding 1 stock, 
        *        2: already sold stock
        */
        val profits = Array.ofDim[Int](prices.length, 3)
        
        profits(0)(0) = 0
        profits(0)(1) = -prices(0)
        profits(0)(2) = Int.MinValue
        
        for (i <- 1 until prices.length) {
            
            profits(i)(0) = profits(i - 1)(0)  // state: 0 -> 0
            profits(i)(1) = profits(i - 1)(1) max (profits(i - 1)(0) - prices(i)) // state: 0 -> 1, 1 -> 1
            profits(i)(2) = profits(i - 1)(2) max (profits(i - 1)(1) + prices(i)) // state: 2 -> 2, 1 -> 2
        }
        profits.last.max
    }
}
/**
* dynamic programming
* time complexity: O(N)
* space complexity: O(1): only create a size 3 of one dimension array
*/
object Solution1-2 {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices == null || prices.isEmpty) return 0
       /* 
       * state: 0: without holding, 
       *        1: holding 1 stock, 
       *        2: already sold stock
       */
        val dp = Array.ofDim[Int](3)
        dp(0) = 0
        dp(1) = -prices(0)
        dp(2) = Int.MinValue // initial as 0 is acceptable
        
        for(i <- 1 until prices.size){
            dp(0) = dp(0)
            dp(1) = (dp(0) - prices(i)) max dp(1)
            dp(2) = (dp(1) + prices(i)) max dp(2)
        }
        dp.max
    }
}


/**
* Kadane's Algorithm: though of dynamic programming
* record min price so far and maxProfit during iteration
* time complexity O(N)
* space complexity O(1)
*/

object Solution2 {
    def maxProfit(prices: Array[Int]): Int = {
        prices.foldLeft((Int.MaxValue, 0)){
            case ((minPriceSoFar, maxProfit), price) => (minPriceSoFar min price, maxProfit max (price - minPriceSoFar))
        }._2
    }
}