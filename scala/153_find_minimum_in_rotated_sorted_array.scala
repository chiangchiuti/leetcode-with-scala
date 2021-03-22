
/**
* my first commitment binary search
*/
object Solution1 {
    def findMin(nums: Array[Int]): Int = {
        search(nums, 0, nums.length - 1)
    }
  
    def search(nums: Array[Int], left: Int, right: Int): Int = {
      if (left > right) return nums(left)
      val mid = left + (right - left) / 2
      val leftAns = if (nums(mid) >= nums(left)){ // left part in order
        nums(left)
      } else {
        search(nums, left, mid - 1)
      }
      
      val rightAns = if (nums(mid) <= nums(right)) { // right part in order
        nums(mid)
      } else {
        search(nums, mid + 1, right)
      }
      
      leftAns min rightAns
    }
}