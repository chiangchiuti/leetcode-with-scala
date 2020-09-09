/**
* using max heap
* pq = pq.filter{case (_v: Int, _idx: Int) => (_v >= v) && (_idx > idx - k)} : keep element's time complexity is O(K)
*/

object Solution {
    def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
        var pq = scala.collection.mutable.PriorityQueue.empty[(Int, Int)](Ordering.by(p  => p._1))
        val rest = scala.collection.mutable.ArrayBuffer[Int]()
        
        nums.zipWithIndex.foreach{case (v: Int, idx: Int) => {
     
            pq += ((v, idx))

            if (idx + 1 >= k) {
                /* keep the elements that is only larger than newest v and the nearest k */
                pq = pq.filter{case (_v: Int, _idx: Int) => (_v >= v) && (_idx > idx - k)}                
                rest += pq.head._1
            }
          
        }}        
        rest.toArray
    }
}


/**
* fold left version, it is worst than for loop version
*/
object Solution {
    def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
      val pq = scala.collection.mutable.PriorityQueue.empty[(Int, Int)](Ordering.by(p  => p._1))
  val (_, rest, _) = nums.zipWithIndex.foldLeft((pq, Array.empty[Int], k)){
    (B, v_id) =>
      val (_pq, rest: Array[Int], _k) = B
      var newPq = _pq
      newPq += v_id
      if(v_id._2 + 1 >= k) {
        newPq = newPq.filter{case (_v:Int, _idx: Int) => (_v >= v_id._1) && (_idx > v_id._2 - k) }
          
         (newPq, rest :+ newPq.head._1, _k)
      }else {
         (newPq, rest, _k)
      }
     
  }
  rest
        
    }
}


/**
* using scala vector, due to scala vector is immutable, any operation about add update remove is generate a new vector
* so it's not a proper substitute for deque
*/

object Solution {
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    var windows = Vector.empty[Int]
    val ret = scala.collection.mutable.ArrayBuffer.empty[Int]

    nums.zipWithIndex.foreach { case (value: Int, index: Int) =>
      if (index >= k && windows.head <= index - k)
        windows = windows.drop(1)

      while (windows.nonEmpty && nums(windows.last) <= value){
        windows = windows.dropRight(1)
      }
      windows = windows :+ index
      if (index + 1 >= k) {
        ret += nums(windows.head)
      }
    }
    ret.toArray
  }
}

/**
* using java array deque version which remove first/last element from collection is O(1)
*/
object Solution {
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    import java.util
    val windows = new util.ArrayDeque[Int]
    val ret = scala.collection.mutable.ArrayBuffer.empty[Int]

    nums.zipWithIndex.foreach { case (value: Int, index: Int) =>
      if (index >= k && windows.peekFirst() <= index - k) {
        windows.removeFirst()
      }

      while (!windows.isEmpty && nums(windows.peekLast()) <= value) {
        windows.removeLast()
      }
      windows.add(index)
      if (index + 1 >= k) {
        ret += nums(windows.peekFirst())
      }
    }
    ret.toArray
  }
}