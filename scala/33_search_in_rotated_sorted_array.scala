

/**
* my first commitment
* binary search
* memo
* 1. check if it is sorted side first. if side is sorted, using the normal binary search function , or else using the search function 
*/

object Solution1 {
    def search(nums: Array[Int], target: Int): Int = {
        search(nums, target, 0 , nums.length - 1)
    }
    def search(nums: Array[Int], target: Int, left: Int, right: Int): Int = {
      if(left > right) return -1
      
      val mid = left + (right - left) / 2
      val midValue = nums(mid)
      
      if (midValue == target) return mid
      
      val leftAns = if (nums(left) < midValue)  
        searchOrder(nums, target, left, mid - 1)
      else 
        search(nums, target, left, mid - 1)
      
      if (leftAns != -1) 
        leftAns
      else {
        if (midValue < nums(right))
           searchOrder(nums, target, mid + 1, right)
        else
          search(nums, target, mid + 1, right)
      } 
    }
  
    def searchOrder (nums: Array[Int], target: Int, left: Int, right: Int): Int = {
      if(left > right) return -1
      val mid = left + (right - left) / 2
      val midValue = nums(mid)
      if (midValue == target) 
       mid
      else if (target > midValue)
        searchOrder(nums, target, mid + 1, right)
      else 
        searchOrder(nums, target, left, mid - 1)
    }
}


/**
* binary search - iterative version
*/
object Solution1-2 {
    def search(nums: Array[Int], target: Int): Int = {
      var left = 0
      var right = nums.length - 1
      
      var ans = -1
      while(ans == -1 && left <= right) {
        val mid = left + (right - left) / 2

        if (target == nums(mid) ){
          ans = mid

        } else if (nums(left) <= nums(mid)){ // left part is in order
          if (nums(mid) > target && target >= nums(left)) { // target is in left part
            right = mid - 1
          } else {
            left = mid + 1
          }
        } else { // right part is in order
          if (nums(mid) < target && target <= nums(right)) { // target is in right part
            left = mid + 1
          } else {
            right = mid - 1
          }
        } 
      }
      ans
    }
}

