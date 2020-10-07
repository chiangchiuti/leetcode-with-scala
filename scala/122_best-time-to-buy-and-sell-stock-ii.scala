/**
* greedy alg
*/
object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices.length > 1){
            prices.sliding(2).collect{ case arr: Array[Int] if arr(1) > arr(0)=> arr}
    .foldLeft(0){(sum, arr) => 
      sum + arr(1) - arr(0)}
        } else {
            0
        }
    }
}

/**
* greedy alg: one line pass
*/

object Solution {
  def maxProfit(prices: Array[Int]): Int = {
    if(prices.length > 1) prices.sliding(2).collect{case arr if arr(1) > arr(0) => arr(1) - arr(0)}.sum else 0
  }
}

/**
* dynamic programming 
*/

object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        /* status definition: 0 for without holding, 1 for holding stocks */
        val profits = Array.ofDim[Int](prices.length, 2)
        
        profits(0)(0) = 0
        profits(0)(1) = -prices(0)
        for(i <- 1 until prices.length) {
            profits(i)(0) = profits(i - 1)(0) max (prices(i) + profits(i - 1)(1)) //  sell 
            profits(i)(1) = profits(i - 1)(1) max (profits(i)(0) - prices(i)) // buy and hold

        }
        profits(profits.length - 1).max
    }
}