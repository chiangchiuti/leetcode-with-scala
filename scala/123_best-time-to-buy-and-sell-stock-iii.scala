/**
* Dynamic programming
*/ 
object Solution {
  def maxProfit(prices: Array[Int]): Int = {
    /* status i:buy times: holding or not */
    val profits = Array.ofDim[Int](prices.length, 3, 2)
    val maxV = prices.max
    profits(0)(0)(0) = 0
    profits(0)(0)(1) = -prices(0)
    profits(0)(1)(0) = -maxV
    profits(0)(1)(1) = -maxV
    profits(0)(2)(0) = -maxV
    profits(0)(2)(1) = -maxV


    for(i <- 1 until prices.length) {
      profits(i)(0)(0) = profits(i - 1)(0)(0)
      profits(i)(0)(1) = profits(i - 1)(0)(1) max (profits(i - 1)(0)(0) - prices(i)) // buy
      profits(i)(1)(0) = profits(i - 1)(1)(0) max (profits(i - 1)(0)(1) + prices(i)) // sell
      profits(i)(1)(1) = profits(i - 1)(1)(1) max (profits(i - 1)(1)(0) - prices(i)) // buy
      profits(i)(2)(0) = profits(i - 1)(2)(0) max (profits(i - 1)(1)(1) + prices(i)) // sell
        
    }

    profits(prices.length - 1).map(_.max).max
  }
}

/**
*  dynamic programming with only keeping two time status: current and previous
* this version is more beautiful than above one
* time complexity: O(N)
* space complexity: O(2 *2 * 2)
*/
object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        val transactions = 2
        val profits = Array.ofDim[Int](2, transactions, 2)
        
        for (i <- profits.indices; j <- 0 until transactions) {
            profits(i)(j)(0) = Int.MinValue // buy
            profits(i)(j)(1) = 0 // sell
        }
     
        
        for (i <- prices.indices; j <- 0 until transactions) {
            val currentStatus = i % 2
            val previousStatus = (i + 1) % 2
            profits(currentStatus)(j)(1) =  profits(previousStatus)(j)(1) max  (profits(previousStatus)(j)(0) + prices(i)) // sell

            if(j == 0)  
                profits(currentStatus)(j)(0) =  profits(previousStatus)(j)(0) max - prices(i) // buy
            else 
                profits(currentStatus)(j)(0) =  profits(previousStatus)(j)(0) max (profits(previousStatus)(j - 1)(1) - prices(i)) // buy

        }

        profits((prices.length - 1) % 2).map(_.max).max
        
    }
     private def debugProfits(profits: Array[Array[Array[Int]]]): Unit = {
        profits.zipWithIndex.foreach{
          case (p, i) =>
            println(s"status: $i")
            p.zipWithIndex.foreach{
            case (pp, j) =>
                println(s"transaction $j: hold: ${pp(0)}, sell: ${pp(1)}")
          }
            println(" ")
        }
  }
}


/**
* dynamic programming only keep one previous status
* time complexity: O(N)
* space complexity: O(1)
*/ 
object Solution1 {
    def maxProfit(prices: Array[Int]): Int = {
        val r = prices.foldLeft((Int.MinValue, 0, Int.MinValue, 0)){
            case (acc, px) =>
                val (buy1, sell1, buy2, sell2) = acc
                val newBuy1 = buy1 max - px
                val newSell1 =  sell1 max (buy1 + px)
                val newBuy2 = buy2 max (sell1 - px)
                val newSell2 = sell2 max (buy2 + px)
                (newBuy1, newSell1, newBuy2, newSell2)
        }
        r._2 max r._4
    }
}