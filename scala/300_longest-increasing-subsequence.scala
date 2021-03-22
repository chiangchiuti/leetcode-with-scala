


/**
* chosen answer
* dynamic programming 
* memo
*   1. dp[i] represent the max length including index i ending at index i
*   2. if nums[j] < nums[i] where j < i, we could increase 1 from dp[j]
*  time complexity: O(N^2)
*  space  complexity: O(N)
*/

object Solution0 {
    def lengthOfLIS(nums: Array[Int]): Int = {
        if(nums == null || nums.isEmpty) return 0
        val dp = Array.fill[Int](nums.length)(1) // record the LIS of 0 to i sub-array in nums while select i
        for(i <- nums.indices; j <- 0 until i) {
            if(nums(i) > nums(j)) {
                dp(i) = (dp(j) + 1) max dp(i)
            }
        }
        dp.max
        
    }
}

/**
* brute force : not Ac
* memo:
* 1. each position have two choice :
*    1. take current value if currentIdx value > previousIdx value 
*    2. do not take current value
* time complexity: O(2^n)
*/
object Solution1 {
    def lengthOfLIS(nums: Array[Int]): Int = {
        lengthOfLIS(nums, 0, -1)
    }
  
    def lengthOfLIS(nums: Array[Int], currentIdx: Int, previousIdx: Int): Int = {
      if (currentIdx >= nums.length) return 0
      
      val taken = if (previousIdx == -1  ||  (nums(currentIdx) > nums(previousIdx))) {
        lengthOfLIS(nums, currentIdx + 1, currentIdx) + 1
      } else {
        0
      } 
      val nonTaken = lengthOfLIS(nums, currentIdx + 1, previousIdx)
      taken max nonTaken
    }
}

/**
* with memorized: we just fill the nxn dimension memory array
* time complexity: O(n^2)
* space complexity: O(n^2)
*/
object Solution1-2 {
    def lengthOfLIS(nums: Array[Int]): Int = {
      val memory = Array.fill[Int](nums.length, nums.length)(-1)
      lengthOfLIS(nums, 0, -1, memory)
    }
  
    def lengthOfLIS(nums: Array[Int], currentIdx: Int, previousIdx: Int, memory: Array[Array[Int]]): Int  = {
      // println(currentIdx, previousIdx)
      if (nums.length == currentIdx) return 0
      if (memory(currentIdx)(previousIdx + 1) != -1) return memory(currentIdx)(previousIdx + 1)
      
      val taken = if (previousIdx == -1 || nums(currentIdx) > nums(previousIdx)) {
        1 + lengthOfLIS(nums, currentIdx + 1, currentIdx, memory)
      } else {
        0
      }
      
      val nonTaken = lengthOfLIS(nums, currentIdx + 1, previousIdx, memory)
      
      memory(currentIdx)(previousIdx + 1) = taken max nonTaken
      
      memory(currentIdx)(previousIdx + 1) 
    }
  
  
}





/**
* dynamic programming 
* memo
*   1. dp[i] represent the max length including index i ending at index i
*   2. if nums[j] < nums[i] where j < i, we could increase 1 from dp[j]
*  time complexity: O(N^2)
*  space  complexity: O(N)
*/

object Solution3 {
    def lengthOfLIS(nums: Array[Int]): Int = {
        if(nums == null || nums.isEmpty) return 0
        val dp = Array.fill[Int](nums.length)(1) // record the LIS of 0 to i sub-array in nums while select i
        

        for(i <- nums.indices; j <- 0 until i) {
            if(nums(i) > nums(j)) {
                dp(i) = (dp(j) + 1) max dp(i)
            }
        }
        dp.max
        
    }
}