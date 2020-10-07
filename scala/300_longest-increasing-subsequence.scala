/**
* dynamic programming 
*  time complexity: O(N^2)
*  space  complexity: O(N)
*/
object Solution {
    def lengthOfLIS(nums: Array[Int]): Int = {
        if(nums == null || nums.isEmpty) return 0
        val dp = Array.fill[Int](nums.length)(1) // record the LIS of 0 to n sub-array in nums while select n
        
        for(i <- 1 until nums.length; j <- 0 until i) {
            if(nums(i) > nums(j)) {
                dp(i) = (dp(j) + 1) max dp(i)
            }
        }
        dp.max
        
    }
}