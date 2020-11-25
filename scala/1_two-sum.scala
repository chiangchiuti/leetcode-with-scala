/**
* chosen solution
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


/**
*  two pointer approximate
*/
object Solution2 {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
        @annotation.tailrec
        def _twoSum(v2Idx: Array[(Int, Int)], left: Int, right: Int): Array[Int] = {
            if(left < right) {

                val sum =  v2Idx(left)._1 + v2Idx(right)._1
                
                if(sum < target)  _twoSum(v2Idx, left + 1, right) 
                else if(sum > target) _twoSum(v2Idx, left, right - 1)    
                else  Array(v2Idx(left)._2, v2Idx(right)._2)
                
            }else  Array()
        }
        
       val v2Idx = nums.zipWithIndex.sortBy(t => t._1)
        _twoSum(v2Idx, 0, v2Idx.length - 1)
        
    }
}