

/**
* my first commitment
* memo
* 1. find the first index i which breaks the increasing order
* 2. find the last index  j which is larger than index i
* 3. swap(i, j)
* 4. sorting: reverse sequence from i + 1 to the end 
* time complexity: O(n)
*/

object Solution1 {
    def nextPermutation(nums: Array[Int]): Unit = {
        /**
        * find the first index i which breaks the increasing order
        * 0 1 2 3 4 5 6
        * 5 4 7 6 5 4 3
        *   i     j 
        */
      ((nums.length - 2) to 0 by -1).find(idx => nums(idx) < nums(idx + 1)) match {
        case Some(idx) => 
          /* 
          * find the last index  j which  is larger than index i
          */
          val j = ((idx + 1) until nums.length).findLast(i => nums(idx) < nums(i)).getOrElse(idx)
          swap(nums, idx, j)
          reverse(nums, idx + 1, nums.length - 1)
        case None => reverse(nums, 0, nums.length - 1)
      }
    }
    @annotation.tailrec
    def reverse(nums: Array[Int], from: Int, to: Int) {
      if (from < to) {
        swap(nums, from, to)
        reverse(nums, from + 1, to - 1)
      }
    }
  
    def swap(nums: Array[Int], index1: Int, index2: Int) {
      val tmp = nums(index2)
      nums(index2) = nums(index1)
      nums(index1) = tmp
    }
}