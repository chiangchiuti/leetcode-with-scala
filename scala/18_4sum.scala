/**
* my first commit
* two pointer in twoSum
* time complexity: O(N^3)
*/
object Solution1 {
  def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
    val l = nums.sorted
    val result = scala.collection.mutable.Set[List[Int]]()

    for (i <- l.indices; j <- (i + 1) until nums.length) {
      val twoSumTarget = target - l(i) - l(j)
      result ++= twoSum(l, twoSumTarget, j + 1).map(List(l(i), l(j)) ::: _)
    }
    result.toList

  }


  def twoSum(nums: Array[Int], target: Int, from: Int): Set[List[Int]] = {

    @annotation.tailrec
    def loop(i: Int, j: Int, ans: Set[List[Int]]): Set[List[Int]] = {
      if (i < j) {
        val sum = nums(i) + nums(j)

        if (target > sum) loop(i + 1, j, ans)
        else if (target < sum) loop(i, j - 1, ans)
        else loop(i + 1, j - 1, ans + List(nums(i), nums(j)))

      } else {
        ans
      }
    }

    loop(from, nums.length - 1, Set[List[Int]]())

  }
}


/**
* kSum template
*/

object Solution2 {
  def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
    val l = nums.sorted
    val k = 4
    kSum(k, l, target, 0).toList
  }

  def kSum(k: Int, nums: Array[Int], target: Int, from: Int): Set[List[Int]] = {
    if (k == 2){
      twoSum(nums, target, from)
    }else {
      val ret = scala.collection.mutable.Set[List[Int]]()
      for (i <- from until nums.length) {
        val v = nums(i)
        ret ++= kSum(k - 1, nums, target - v, i + 1).map(v +: _)
      }
      ret.toSet
    }
  }


  def twoSum(nums: Array[Int], target: Int, from: Int): Set[List[Int]] = {

    @annotation.tailrec
    def loop(i: Int, j: Int, ans: Set[List[Int]]): Set[List[Int]] = {
      if (i < j) {
        val sum = nums(i) + nums(j)

        if (target > sum) loop(i + 1, j, ans)
        else if (target < sum) loop(i, j - 1, ans)
        else loop(i + 1, j - 1, ans + List(nums(i), nums(j)))

      } else {
        ans
      }
    }
    loop(from, nums.length - 1, Set[List[Int]]())

  }
}