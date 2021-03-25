/**
* my first commitment
* memo
*  1. insert newInterval to intervals according to its first element value
*  2. combine overlapping range 
* time complexity: O(2N) = O(N)
* space complexity: O(N)
*/

object Solution1-1 {
    def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
      if (intervals.isEmpty) return Array(newInterval)
      (_insert(_, newInterval)).andThen(combine).apply(intervals).reverse.toArray
    }  
    val _insert = (intervals: Array[Array[Int]], newInterval: Array[Int]) => {
    /**
    * find the position to split intervals into two parts
    */
      val pos = intervals.indices.find(idx => intervals(idx).head > newInterval.head).getOrElse(intervals.length)
      intervals.slice(0, pos).toList ::: List(newInterval) ::: intervals.slice(pos, intervals.length).toList
    }
  
    val combine = (input: List[Array[Int]])  => input.foldLeft(List.empty[Array[Int]]) {
        case (last::ans, arr) =>
          if (last.last < arr.head) arr::last::ans
          else Array(last.head, arr.last max last.last)::ans
        case (ans, arr) => //for empty ans
            arr::ans
      }
}

/**
* optimize from 1-1
* 1.span
*/
object Solution1-2 {
    def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
      // if (intervals.isEmpty) return Array(newInterval)
      (insert(_, newInterval)).andThen(combine).apply(intervals.toList).reverse.toArray
    }
  
    val insert = (intervals: List[Array[Int]], newInterval: Array[Int]) => {
      val (a, b) = intervals.span(arr => arr.head < newInterval.head)
      a:::List(newInterval):::b
    }
  
    val combine = (input: List[Array[Int]])  => input.foldLeft(List.empty[Array[Int]]) {
        case (last::ans, arr) => if (last.last < arr.head) arr::last::ans else Array(last.head, arr.last max last.last)::ans
        case (ans, arr) => arr::ans //for empty ans
      }
}