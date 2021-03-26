

/**
* my first commitment
*
*/
object Solution1 {
    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
      val hit = search(nums, target, 0, nums.length - 1)
      if (hit == -1)
        Array(-1, -1)
      else  {
        var left = hit
        while (left - 1 >= 0 && nums(left) == nums(left - 1)){
          left -= 1
        }
        var right = hit
        while(right + 1 < nums.length && nums(right) == nums(right + 1)){
          right += 1
        }
        Array(left, right)
      }
    }
  
    @annotation.tailrec
    def search(nums: Array[Int], target: Int, left: Int, right: Int): Int = {
      if (left > right) return -1
      
      val mid = left + (right - left) / 2
       
      if (nums(mid) == target)
        mid
      else if (nums(mid) > target)
        search(nums, target, left, mid - 1)
      else 
        search(nums, target, mid + 1, right)
      
      
    }
 }

/**
* function programming
*/

 object Solution1-2 {
    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
      val hit = search(nums, target, 0, nums.length - 1)
      if (hit == -1)
        Array(-1, -1)
      else {
        val left = (hit to 0 by -1 ).findLast(l => nums(l) == nums(hit)).getOrElse(-1)
        val right = (hit to (nums.length - 1)).findLast(r => nums(r) == nums(hit)).getOrElse(-1)
        Array(left, right)
      } 
    }
    @annotation.tailrec
    def search(nums: Array[Int], target: Int, left: Int, right: Int): Int = {
      if (left > right) return -1
      val mid = left + (right - left) / 2
       
      if (nums(mid) == target)
        mid
      else if (nums(mid) > target)
        search(nums, target, left, mid - 1)
      else 
        search(nums, target, mid + 1, right)
    }
 }

/**
* modify binary search template
* memo
*  1. search first and last the the same function
*  2. if nums(mid) == target we could move left to check if left part exists target number
*  3. finding last by target + 1,  then we could get last position of target by first position of (target + 1) - 1
* tricky:
*  1. ans = nums.length
*  2. first > last  means that target doesn't exists
*
* time complexity: O(2logN)
*/
 
 object Solution2 {
    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
        val first = search(nums, target)
        val last = search(nums, target + 1) - 1
        if (first > last) Array(-1, -1) else Array(first, last)
    }

    def search(nums: Array[Int], target: Int): Int = {
      var ans = nums.length
      var left = 0
      var right = nums.length - 1
      while (left <= right) {
        val mid = left + (right - left) / 2
        if (nums(mid) >= target) {
          ans = mid
          right = mid - 1
        }else {
          left = mid + 1
        } 
      }
      ans
    }
}

/**
* recursive version
*/
object Solution2-1 {
    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
      val first = search(nums, target, 0, nums.length - 1, nums.length)
      val last = search(nums, target + 1, 0, nums.length - 1, nums.length) - 1
      if (first > last) Array(-1, -1) else Array(first, last)
    }
  
    @annotation.tailrec
    def search(nums: Array[Int], target: Int, left: Int, right: Int, ans: Int): Int = {
      if (left > right) return ans
      val mid = left + (right - left) / 2
      
      if (nums(mid) == target)
        search(nums, target, left, mid - 1, mid)
      else if (nums(mid) > target)
        search(nums, target, left, mid - 1, mid)
      else
        search(nums, target, mid + 1, right, ans)
      
    }
}
