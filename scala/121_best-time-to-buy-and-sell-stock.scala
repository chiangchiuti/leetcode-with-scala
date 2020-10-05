/**
* dynamic programming
* time complexity : O(N)
* space complexity: O(3N)
*/
object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        if (prices == null || prices.isEmpty) return 0
        
        var ret = 0
        /* status: 0: without holding, 1: holding 1 stock, 2: already sell stocks */
        val profits = Array.ofDim[Int](prices.length, 3)
        
        profits(0)(0) = 0
        profits(0)(1) = -prices(0)
        profits(0)(2) = 0
        
        for (i <- 1 until prices.length) {
            
            profits(i)(0) = profits(i - 1)(0)  // status: 0 -> 0
            profits(i)(1) = profits(i - 1)(1) max (profits(i - 1)(0) - prices(i)) // status: 0 -> 1, 1 -> 1
            profits(i)(2) = profits(i - 1)(2) max (profits(i - 1)(1) + prices(i)) // status: 2 -> 2, 1 -> 2
        }
        profits(profits.length - 1)(2)
    }
}



/**
* record min price so far and maxProfit during iteration
* time complexity O(N)
* space complexity O(1)
*/

object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        prices.foldLeft((Int.MaxValue, 0)){
            case ((minPriceSoFar, maxProfit), price) => (minPriceSoFar min price, maxProfit max (price - minPriceSoFar))
        }._2
    }
}