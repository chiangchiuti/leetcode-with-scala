/**
* select solution
* time complexity: O(N)
*/


object Solution0 {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val value2Idx = nums.zipWithIndex.toMap
    nums.zipWithIndex.collectFirst {
      case (value, index) if value2Idx.get(target - value).exists(_ != index) =>
        Array(index, value2Idx(target - value))
    }.get
  }
}

/**
* HashTable
* time complexity: O(N)
*/

object Solution1 {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val value2Idx = nums.zipWithIndex.toMap
    val ret = collection.mutable.ArrayBuffer[Int]()

    for ((n, idx) <- nums.zipWithIndex; if ret.length < 2) {
      val v2 = target - n
      value2Idx.get(v2) match {
        case Some(v2Idx) if v2Idx != idx =>
          ret ++= Array(idx, v2Idx)
        case _ =>
      }
    }
    ret.toArray
  }
}


/**
* more elegant
*/


object Solution1-2 {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val value2Idx = nums.zipWithIndex.toMap
    nums.zipWithIndex.collectFirst {
      case (value, index) if value2Idx.get(target - value).exists(_ != index) =>
        Array(index, value2Idx(target - value))
    }.get
  }
}
