/**
* my first commitment
* dynamic programming
*     dp(i)(j) means the best profit we can have at i-th day in different state un-holding stock or holding a share of stock.
* 
*  memo:
*    this problem is similar to problem no 122
*  time complexity: O(N)
*  space complexity: O(N)
*/
object Solution1 {
    def maxProfit(prices: Array[Int], fee: Int): Int = {
        if(prices == null || prices.isEmpty) return 0
        
        /*
        *  0 for un-holding any stack
        *  1 for holding a share of stock
        */
        val dp = Array.tabulate(prices.length, 2) {
            case (0, 0) => 0
            case (0, 1) => -prices(0)
            case _ => 0
        }
        
        // 0: without holding, 1 holding
        for(i <- 1 until prices.length) {
            /** only pay the transition fee in selling a share of stock */
            dp(i)(0) = dp(i - 1)(0) max (dp(i - 1)(1) + prices(i) - fee)
            dp(i)(1) = dp(i - 1)(1) max (dp(i - 1)(0) - prices(i))
        }
        dp.last(0) // last time's state 0
        
    }
}

/**
* dynamic programming: only create an array keeping holding and un-holding
* time complexity: O(N)
* space complexity:  O(1)
*/
object Solution1-1 {
    def maxProfit(prices: Array[Int], fee: Int): Int = {
        if(prices == null || prices.isEmpty) return 0
        val dp = Array.ofDim[Int](2)
        dp(0) = 0
        dp(1) = -prices(0)
        // 0 un-holding, 1 holding
        for(i <- 1 until prices.length){
        /*
        * it may causes a problem here, because we overwrite the previous dp(0) by new state i value and dp(1) would utilizes dp(0) which was overwritten 
        */
            dp(0) = dp(0) max (dp(1) + prices(i) - fee)
            dp(1) = dp(1) max (dp(0) - prices(i))
        }
        dp(0)
    }
}

/**
* dynamic programming 
* function programming
*/
object Solution2 {
    def maxProfit(prices: Array[Int], fee: Int): Int = {
        val (unholding, holding) = prices.foldLeft((0, Int.MinValue)){
            case ((unholding, holding), price) =>
            (
            // avoiding overflow
                if((price - fee) > 0) unholding max (holding + price - fee) else unholding,
                holding max (unholding - price)
            )
        }
        unholding
    }
}