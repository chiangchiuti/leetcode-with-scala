/**
* Dynamic programming: three dimension dp array
*    memo:
*       dp definition: dp[i][j][l] means the best profit we can have at i-th day using EXACT j transactions and with/without stocks in hand.
*/ 
object Solution1 {
  def maxProfit(prices: Array[Int]): Int = {
    /* 
    * profits(i)(j)(k)
    *   dimension i: state sequence
    *   
    *   profits()(0)(0) keep observing
    *   profits()(0)(1) buy first share
    *   profits()(1)(0) after selling first share
    *   profits()(1)(1) buy second share
    *   profits()(2)(0) after selling second share
    *   profits()(2)(1) non-meaningful
    */
    val profits = Array.ofDim[Int](prices.length, 3, 2)
    
    profits(0)(0)(0) = 0
    profits(0)(0)(1) = -prices(0)
    profits(0)(1)(0) = 0
    profits(0)(1)(1) = Int.MinValue  // buy state
    profits(0)(2)(0) = 0
    profits(0)(2)(1) = Int.MinValue // buy state
    
    /* state transition */
    for(i <- 1 until prices.length) {
      profits(i)(0)(0) = profits(i - 1)(0)(0)  // actually non-meaningful
      profits(i)(0)(1) = profits(i - 1)(0)(1) max (profits(i - 1)(0)(0) - prices(i)) // buy
      profits(i)(1)(0) = profits(i - 1)(1)(0) max (profits(i - 1)(0)(1) + prices(i)) // sell
      profits(i)(1)(1) = profits(i - 1)(1)(1) max (profits(i - 1)(1)(0) - prices(i)) // buy
      profits(i)(2)(0) = profits(i - 1)(2)(0) max (profits(i - 1)(1)(1) + prices(i)) // sell        
    }
    profits.last.map(_(0)).max
  }
}
/**
* Dynamic programming: three dimension dp array
*   shift state definition
*/ 
object Solution1-2 {
    def maxProfit(prices: Array[Int]): Int = {
            /* 
            * profits(i)(j)(k)
            *   dimension i: state sequence
            *   
            *   profits()(0)(0) dummy state
            *   profits()(0)(0) dummy state
            *   profits()(1)(0) buying first share
            *   profits()(1)(1) after sold first share
            *   profits()(2)(0) buying second share
            *   profits()(2)(1) after sold second share
            */
        val dp = Array.tabulate(prices.length, 3, 2){
            case (0, 1, 0) => -prices(0)  // buy state
            case (0, 1, 1) => 0  // sell state
            case (0, 2, 0) => Int.MinValue // buy state
            case (0, 2, 1) => 0 // sell state
            case _ => 0
        }
        for(i <- 1 until prices.length; j <- 1 to 2) {
            dp(i)(j)(0) = dp(i - 1)(j)(0) max (dp(i - 1)(j - 1)(1)  - prices(i)) // buy
            dp(i)(j)(1) = dp(i - 1)(j)(1) max (dp(i - 1)(j)(0) + prices(i))  // sell
        }
        dp.last.map(_(1)).max
    }
}
/**
* dynamic programming: tree dimension array
*   drop dummy state
*/

object Solution1-3 {
    def maxProfit(prices: Array[Int]): Int = {
        val transactionLimit = 2
            /* 
            * profits(i)(j)(k)
            *   dimension i: state sequence
            *   profits()(0)(0) buying first share
            *   profits()(0)(1) after sold first share
            *   profits()(1)(0) buying second share
            *   profits()(1)(1) after sold second share
            */
        val dp = Array.tabulate(prices.length, transactionLimit, 2){
            case (0, 0, 0) => -prices(0)  // buy
            case (0, _, 1) => 0  // sell
            case (0, _, 0) => Int.MinValue // buy
            case _ => 0
        }
        
        for(i <- 1 until prices.length; j <- 0 until transactionLimit) {
            /*
            * 0 buy, 1 sell
            */
            dp(i)(j)(0) = dp(i - 1)(j)(0) max {
                if(j == 0) -prices(i)
                else dp(i - 1)(j - 1)(1) - prices(i)
            }    
            dp(i)(j)(1) = dp(i - 1)(j)(1) max (dp(i - 1)(j)(0) + prices(i))
        }
        dp.last.map(_(1)).max
    }
}

/**
* Dynamic programming with only keeping two time state: current and previous
* this version is more elegant than above one
* time complexity: O(N)
* space complexity: O(2 * 2 * 2) = O(8) = O(1)
*/
object Solution2 {
    def maxProfit(prices: Array[Int]): Int = {
        val transactions = 2
        val profits = Array.ofDim[Int](2, transactions, 2)
        
        for (i <- profits.indices; j <- 0 until transactions) {
            profits(i)(j)(0) = Int.MinValue // buy
            profits(i)(j)(1) = 0 // sell
        }
     
        /** iterate from index 0 */
        for (i <- prices.indices; j <- 0 until transactions) {
            val currentStatus = i % 2
            val previousStatus = (i + 1) % 2
            profits(currentStatus)(j)(1) =  profits(previousStatus)(j)(1) max  (profits(previousStatus)(j)(0) + prices(i)) // sell

            if(j == 0)  
                profits(currentStatus)(j)(0) =  profits(previousStatus)(j)(0) max - prices(i) // buy
            else 
                profits(currentStatus)(j)(0) =  profits(previousStatus)(j)(0) max (profits(previousStatus)(j - 1)(1) - prices(i)) // buy from previous (j - 1) sell status

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
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution2-1 {
    def maxProfit(prices: Array[Int]): Int = {
        val transactionLimit = 2
        val dp = Array.tabulate(2, transactionLimit, 2) {
            case (_, _, 0) => Int.MinValue
            case (_, _, 1) => 0
            case _ => 0
        }
        
        for(i <- prices.indices; j <- 0 until transactionLimit) {
            val currentIdx = i & 1  // bit op: AND op
            val previousIdx = currentIdx ^1 // bit op: XOR op

            // 0 buy; 1 sell
            dp(currentIdx)(j)(0) = dp(previousIdx)(j)(0) max {
                if(j == 0) -prices(i)
                else dp(previousIdx)(j - 1)(1) - prices(i)
            }
            dp(currentIdx)(j)(1) = dp(previousIdx)(j)(1) max (dp(previousIdx)(j)(0) + prices(i))
        }
        
        dp((prices.length - 1) & 1).map(_(1)).max
        
    }
}

/**
* Kadane's Algorithm:  dynamic programming only keep one previous status
* time complexity: O(N)
* space complexity: O(1)
*/ 
object Solution3{
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

object Solution3-1 {
    def maxProfit(prices: Array[Int]): Int = {
        val (buy1, sell1, buy2, sell2) = prices.foldLeft((Int.MinValue, 0, Int.MinValue, 0)){
            case ((buy1, sell1, buy2, sell2), cost) =>
                (
                    buy1 max -cost,
                    sell1 max (buy1 + cost),
                    buy2 max (sell1 - cost),
                    sell2 max (buy2 + cost)
                )
        }
        sell1 max.sell2
    }
}