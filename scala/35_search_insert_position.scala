
/**
* my first commitment
*/
object Solution1 {
    def searchInsert(nums: Array[Int], target: Int): Int = {
        search(nums, target)
    }
    
    def search(nums: Array[Int], target: Int): Int = {
      var left = 0
      var right = nums.length - 1
      var ans = -1
      while(ans == -1 && left <= right) {
        val mid = left + (right - left) / 2
        
        if (nums(mid) == target)
          ans = mid
        else if (nums(mid) > target)
          right = mid - 1
        else
          left = mid + 1
        
      }
      if (ans == -1) left else ans
    }
}