
/**
* my first commitment - binary search
* 
*/
object Solution1 {
    def search(nums: Array[Int], target: Int): Boolean = {
        search(nums, target, 0, nums.length -1) match {
          case -1 => false
          case _ => true
        }
    }
  
    def search(nums: Array[Int], target: Int, left: Int, right: Int): Int = {
      if (left > right) return -1
      val mid  = left + (right - left) / 2
      
      if (target == nums(mid))
        return mid
      
      val leftAns = if (nums(mid) > nums(left)) 
          searchInorder(nums, target, left, mid - 1)
        else
          search(nums, target, left, mid - 1)
      
      if (leftAns  != -1)
        leftAns
      else {
        if (nums(mid) < nums(right))
          searchInorder(nums, target, mid + 1, right)
        else
          search(nums, target, mid + 1, right)
        
      }
    }
  
  
    def searchInorder(nums: Array[Int], target:Int, left: Int, right: Int): Int = {
      if (left > right) return -1
      
      val mid = left + (right - left) / 2
      
      if (target == nums(mid))
        return mid
      else if (target > nums(mid))
        searchInorder(nums, target, mid + 1, right)
      else
        searchInorder(nums, target, left, mid - 1)
      
    }
}