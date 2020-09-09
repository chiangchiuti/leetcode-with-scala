/**
* using min heap
*/
class KthLargest(_k: Int, _nums: Array[Int]) {
    private val pq = scala.collection.mutable.PriorityQueue.empty[Int](Ordering[Int].reverse)
    val k = _k
    _nums.foreach(add)

    def add(`val`: Int): Int = {
        if (pq.size < k)
            pq += `val`
        else if(pq.head < `val`){
            pq.dequeue
            pq += `val`
        }
        // println(pq.clone.dequeueAll)
        pq.head
               
    }
}

/**
 * Your KthLargest object will be instantiated and called as such:
 * var obj = new KthLargest(k, nums)
 * var param_1 = obj.add(`val`)
 */