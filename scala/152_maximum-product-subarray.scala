
/**
* chosen solution
* dynamic programming
* using dp array to record previous max min value ending at index i-th
*   dp(i)(j) means the maximum and minimum contiguous product ending at i-th position
*   each state i update 
*        1. max(current v,  previous state max value * current value,  previous state min value * current value)
*        2. min(current v,  previous state max value * current value,  previous state min value * current value)
*
*  time complexity: O(N)
*/ 

object Solution0 {
  def maxProduct(nums: Array[Int]): Int = {
    val dp = Array.ofDim[Int](nums.length, 2) // record each position n's max product( from 0 to n)
    var result = nums(0)
    dp(0)(0) = nums(0)
    dp(0)(1) = nums(0)

    /* 0 for min, 1 for max */
    for (i <- 1 until nums.length) {
      val a = dp(i - 1)(0) * nums(i)
      val b = dp(i - 1)(1) * nums(i)
      dp(i)(0) = nums(i) min a min b // record min
      dp(i)(1) = nums(i) max a max b // record max
      result = result max dp(i)(1)
    }
    result
  }
}

/**
* my first commitment
* recursive version : correct but may cause memory exceed limit
* time complexity: O(N^2)
*/
object Solution1 {
  def maxProduct(nums: Array[Int]): Int = {
    (1 to nums.length).map(n =>  _maxProduct(nums(n - 1), nums.takeRight(nums.length - n))).max
  }

  def _maxProduct(curr: Int, nums: Array[Int]): Int = {
      if(nums.isEmpty) return curr          
      curr max  _maxProduct( curr * nums(0), nums.takeRight(nums.length - 1))
  }
}


/**
* optimize above one
* don't copy subArray during transmit parameters
* time complexity： O(N^2)
*/
object Solution1-2 {
  def maxProduct(nums: Array[Int]): Int = {
    (1 to nums.length).map(n =>  _maxProduct(nums(n - 1), n, nums)).max
  }
  def _maxProduct(curr: Int, idx: Int, nums: Array[Int]): Int = {
      if(idx >= nums.length) return curr   
      curr max  _maxProduct( curr * nums(idx), idx + 1, nums)
  }

}

/**
* dynamic programming
* using dp array to record previous max min value ending at index i-th
*   dp(i)(j) means the maximum and minimum contiguous product ending at i-th position
*   each state i update 
*        1. max(current v,  previous state max value * current value,  previous state min value * current value)
*        2. min(current v,  previous state max value * current value,  previous state min value * current value)
*
*  time complexity: O(N)
*  space  complexity: O(2N), actually it can be optimized to O(2) which records previous min and max value
*/
object Solution2 {
    def maxProduct(nums: Array[Int]): Int = {
        // 0:  minimum , 1:  maximum
        val dp = Array.ofDim[Int](nums.length, 2)
        dp(0)(0) = nums(0)
        dp(0)(1) = nums(0)
        
        for(i <- 1 until nums.length) {
            val a = dp(i - 1)(0) * nums(i) 
            val b = dp(i - 1)(1) * nums(i)
            dp(i)(0) = a min b min nums(i)
            dp(i)(1) = a max b max nums(i)
        }
        
        
        dp.map(_(1)).max
    }
}


/**
* dynamic programming
* memo
*   1. only keep previous state 
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution2-1 {
    def maxProduct(nums: Array[Int]): Int = {
        
        val (_, _, ans) = (1 until nums.length).foldLeft((nums.head, nums.head, nums.head)){
            case ((min, max, ans), idx) => 
                val a = nums(idx) * min 
                val b = nums(idx) * max
                val newMin = a min b min nums(idx)
                val newMax = a max b max nums(idx)
                (newMin, newMax, ans max newMax)
        }
        ans
    }
}


/**
* a recursive dp method： not my own 
* memo
*   1. only keep the closest state
* time complexity: O(N)
* space complexity: O(N) although it don;t create a length of nums array, it convert nums array to list
*/
object Solution2-2 {
    def maxProduct(nums: Array[Int]): Int = {
        if (nums == null || nums.size == 0) {
            return 0;
        }
        val list: List[Int] = nums.toList
        val head: Int = list.head
        val tail: List[Int] = list.tail
        _MaxProduct(tail, head, head, head)
    }
    
    def _MaxProduct(nums: List[Int], min: Int, max: Int, result: Int): Int = nums match {
        case Nil => result
        case x :: xs => {
            val cur_min = math.min(x, math.min(x * max, x * min))
            val cur_max = math.max(x, math.max(x * max, x * min))
            _MaxProduct(xs, cur_min, cur_max, math.max(cur_max, result))
        }
    }
}