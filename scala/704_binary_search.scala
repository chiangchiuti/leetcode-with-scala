
/**
* my first commitment:
* time complexity: O(logn)
*/

object Solution1 {
    def search(nums: Array[Int], target: Int): Int = {
      var left = 0
      var right = nums.length - 1
      var ans = -1
      while(ans == -1 && left <= right) {
        println(left, right)
        val mid: Int = left  + (right - left) / 2
        if(nums(mid) == target){
          ans = mid
        } else if(target > nums(mid)) {
          left = mid + 1
        } else {
          right = mid - 1
        }
 
      }
      ans
    }
}

/**
* recursive version
*/
object Solution1-2 {
    def search(nums: Array[Int], target: Int): Int = {
        search(nums, target, 0, nums.length - 1)
    }
  
    @annotation.tailrec
    def search(nums: Array[Int], target: Int, left: Int, right: Int): Int = {
      if(left > right) return -1
      
      val mid = left + (right - left) / 2
      if (nums(mid) == target) 
        mid
      else if (target > nums(mid))
        search(nums, target, mid + 1, right)
      else 
        search(nums, target, left, right - 1)
      
    }
}