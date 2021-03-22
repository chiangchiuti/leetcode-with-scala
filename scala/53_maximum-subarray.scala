/**
* chosen solution
* dynamic programming
*    dp[i] defined as the sum of subarray that ending with ith element and must contains i-th element number   *
* actually, we don't need storing all previous status of nums.length
* we just need two status: one for maximum so far, the other one for the maximum accumulated value which containing with nums[i]
*
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution0{
    def maxSubArray(nums: Array[Int]): Int = {
        if (nums == null || nums.isEmpty) return 0
        var maxSoFar = nums(0)
        var maxEndingHere = nums(0)

        for(i <- 1 until nums.length) {
           maxEndingHere = (maxEndingHere +  nums(i))  max nums(i)
           maxSoFar = maxEndingHere max  maxSoFar
        }
        maxSoFar
        
    }
}

/**
* my first commit version
* time complexity: O(N^2)
* space complexity: O(N)
*/

object Solution1 {
    def maxSubArray(nums: Array[Int]): Int = {
     
        (1 to nums.length).map(n => _maxSubArray(nums, nums(n - 1), n)).max
        
    }
    
    def _maxSubArray(nums: Array[Int], preSum: Int, currentIdx: Int): Int = {
        if(nums.length == currentIdx) return preSum
        
        val currentSum = preSum + nums(currentIdx)
        val nexLevelSum = _maxSubArray(nums, currentSum, currentIdx + 1)
        preSum max currentSum max nexLevelSum
    }
    
}

/**
* dynamic programming
* memo:
*    1. dp[i] defined as the sum of subarray that ending with ith element and must contains i-th element number   
* time complexity: O(N)
* space complexity: O(N)  due to dp array
*/

object Solution2 {
    def maxSubArray(nums: Array[Int]): Int = {
        if(nums == null || nums.isEmpty) return 0
        val dp = Array.ofDim[Int](nums.length, 2)  // dp(0) ... dp(i) storing each status corresponding to  nums' index, means max subarray sum ending with nums[i]
        dp(0)(0) = nums(0)  // dim0: accumulate calculator which reset while new element is larger value inside,
        dp(0)(1) = nums(0) // dim1: maximum so far
        
        for(i <- 1 until nums.length) {
            
            dp(i)(0) = (dp(i - 1)(0) + nums(i))  max nums(i)
            dp(i)(1) = dp(i)(0) max dp(i - 1)(1) 
        }
        dp.last.last
    }
}

/**
* dynamic programming
* memo
*   1. one dimension array
* time complexity O(N)
* space complexity O(N)
*/
object Solution2-1 {
    def maxSubArray(nums: Array[Int]): Int = {
      val dp  = Array.ofDim[Int](nums.length)
      dp(0) = nums(0)
      for (i <- 1 until nums.size) {
        dp(i) = nums(i) max (nums(i) + dp(i - 1))
      }
      
      dp.max
    }
}

/**
* dynamic programming
* actually, we don't need storing all previous status of nums.length
* we just need two status: one for maximum so far, the other one for the maximum accumulated value which containing with nums[i]
*
* time complexity: O(N)
* space complexity: O(1)
*/

object Solution2-2 {
    def maxSubArray(nums: Array[Int]): Int = {
        if (nums == null || nums.isEmpty) return 0
        var maxSoFar = nums(0)
        var maxEndingHere = nums(0)

        for(i <- 1 until nums.length) {
           maxEndingHere = (maxEndingHere +  nums(i))  max nums(i)
           maxSoFar = maxEndingHere max  maxSoFar
        }
        maxSoFar
        
    }
}
/**
*  functional programming: foldLeft
*/
object Solution2-3 {
    def maxSubArray(nums: Array[Int]): Int = {
      if(nums == null || nums.isEmpty) return 0
      (1 until nums.length).foldLeft((nums(0), nums(0))){
          case ((maxEndingI, maxSofar), i) => 
            val maxEndingT = nums(i) max (nums(i) + maxEndingI)
            (maxEndingT, maxSofar max maxEndingT )
      }._2
    }
}