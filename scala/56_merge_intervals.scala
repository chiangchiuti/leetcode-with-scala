
/**
*  my first commitment: sort array
*  time complexity: O(nlogn) + O(n) = O(nlogn) 
*  space complexity: O(n): sorted array
*/

object Solution1-1 {
    def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
      val sortedL = intervals.sortBy(_(0))
      val ans = collection.mutable.Set.empty[Array[Int]]
      
      var begin = sortedL(0)(0)
      var end = sortedL(0)(1)
      (1 to sortedL.length - 1).foreach { idx =>
        val l = sortedL(idx)
        if (end < l(0)){
          ans += Array(begin, end)
          begin = l(0)
          end = l(1) 
        }else {
          end = l(1) max end
        }
      }
      ans += Array(begin, end)
      ans.toArray
    }
}

/**
* simplify 1-1
* 1.not using Set
* 2. record uncertain (begin, end) pair in answer list
*/

object Solution1-2 {
    def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
      intervals.sortBy(_(0)).foldLeft(List.empty[Array[Int]]){
        case (last::ans, arr) =>
          if (last.last < arr.head) {
            arr::last::ans
          } else {
            Array(last.head, last.last max arr.last)::ans
          }
        case (ans, arr) => arr::ans // for empty ans list
      }.toArray
    }
}