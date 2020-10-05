/**
* a very time consuming version
* O(N^2)
*/
object Solution {
  def threeSum(nums: Array[Int]): List[List[Int]] = {

      val l = nums.groupBy(identity).mapValues(aa => if(aa.length >=3) aa.take(3) else aa ).values.flatten.toList

     l.zipWithIndex.flatMap {
      case (value, index) =>
        val ll = collection.mutable.ListBuffer(l: _*)
        ll.remove(index)

        twoSum(ll.toList, -value).filter(_.nonEmpty)
          .map(_ :+ value)
    }.map(pair => (pair.toSet, pair)).toMap.values.toList

  }

   def twoSum(nums: List[Int], target: Int): List[List[Int]] = {
    val valueCounter = nums.groupBy(identity).mapValues(_.length)

    nums.collect {
      case value if target - value == value && valueCounter.get(target - value).exists(_ >= 2) =>
        List(value, target - value)
      case value if target - value != value && valueCounter.contains(target - value) =>
        List(value, target - value)

    }
  }

}

/**
* faster than above
* O(N^2)
*/
object Solution2 {
  def threeSum(nums: Array[Int]): List[List[Int]] = {
   
    val l = nums.sorted
    val ret = for((value, index) <- l.zipWithIndex; if index >= 1 && l(index) != l(index - 1)) yield  {
      val ll = l.toBuffer
      ll.remove(index)
      twoSum(ll.toArray, -value).filter(_.nonEmpty).map(_ :+ value)
    }

    l.slice(0, 3) match {
      case Array(0, 0, 0 ) =>  ret.flatten.map(l => (l.toSet, l)).toMap.values.toList :+ List(0, 0, 0)
      case _ => ret.flatten.map(l => (l.toSet, l)).toMap.values.toList
    }

  }

  def twoSum(nums: Array[Int], target: Int): List[List[Int]] = {
    val value2Idx = nums.zipWithIndex.toMap
    nums.zipWithIndex.collect {
      case (value, index) if value2Idx.get(target - value).exists(_ != index) =>

        List(value, target - value)
    }.map(l => (l.toSet, l)).toMap.values.toList
  }

/**
* more faster than above
* improvement:
*   1. only call twoSum when  l(idx) under zero,  because the array was sorted, there won't be any chance the next entries sum to 0.
*   2. only send the remaining nums which were after idx into twoSum
* O(N^2)
*/

  object Solution3 {
    def threeSum(nums: Array[Int]): List[List[Int]] = {
        val l = nums.sorted
        l.indices.foldLeft(collection.mutable.ListBuffer.empty[List[Int]]){
        case (r, idx) if l(idx) <=0 && (idx == 0 || (idx > 0 && l(idx) != l(idx-1))) =>
            r ++= twoSum(l.slice(idx + 1, l.length), -l(idx)).map(_ :+ l(idx))
        case (r, idx)  => r

        }.toList
        
    }

    def twoSum(nums: Array[Int], target: Int): List[List[Int]] = {

        val value2Idx = nums.zipWithIndex.toMap
        nums.zipWithIndex.collect {
        case (value, index) if value2Idx.get(target - value).exists(_ != index) =>
            List(value, target - value)
        }.map(l => (l.toSet, l)).toMap.values.toList
    }
  
}


/**
*  Using a hashset to erase duplicate  in twoSum
*/
object Solution4 {
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val l = nums.sorted
    l.indices.foldLeft(collection.mutable.ListBuffer.empty[List[Int]]){
      case (r, idx) if l(idx) <=0 && (idx == 0 || (idx > 0 && l(idx) != l(idx-1))) =>
        r ++= twoSum(l.slice(idx + 1, l.length), -l(idx))
      case (r, idx)  => r

    }.toList

  }

  def twoSum(nums: Array[Int], target: Int): List[List[Int]] = {

    val value2Idx = nums.zipWithIndex.toMap
    nums.zipWithIndex.foldLeft(Set[List[Int]]()) {
      case (s, (value, index)) if value2Idx.get(target - value).exists(_ != index) =>
        val t_sub_v = target - value
        if(index < value2Idx(t_sub_v)) {
          s + List(-target, value, t_sub_v)
        } else {
          s + List(-target, t_sub_v, value)
        }
      case (s, _) => s

    }.toList
  }
}
