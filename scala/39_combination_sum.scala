/**
* my first commitment: dfs - backtracking
*/

object Solution1-1 {
    import collection.mutable
    def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
      
      def dfs(combination: List[Int], ans: mutable.Set[List[Int]]): Unit = {
        val currentSum = combination.sum
        
        if (currentSum == target) {
          ans += combination.toList
          
        } else if (currentSum < target){
          val diff = target - currentSum
          candidates.filter(n => n <= diff).foreach{ case n => dfs(n :: combination, ans)}
        }
      }
      val ans = mutable.Set.empty[List[Int]]
      dfs(List.empty[Int], ans)
      ans.map(l => l.groupBy(identity).mapValues(_.length).toMap -> l).toMap.values.toList // distinct 
    }
}

/**
* optimize from 1-1: sort combination before appending to ans
*/
object Solution1-2 {
    import collection.mutable
    def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
      
      def dfs(combination: List[Int], currentSum: Int, ans: mutable.Set[List[Int]]): Unit = {
        
        if (currentSum == target) {
          ans += combination.sorted.toList
          
        } else if (currentSum < target){
          val diff = target - currentSum
          candidates.filter(n => n <= diff).foreach{ case n => dfs(n :: combination, currentSum + n, ans)}
        }
      }
      val ans = mutable.Set.empty[List[Int]]
      dfs(List.empty[Int], 0, ans)
      ans.toList
    }
}

/**
* optimize from 1-2: pruning some case- recording candidates array index i 
*/
object Solution1-3{
    import collection.mutable
    def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
      
      def dfs(i: Int, combination: List[Int], currentSum: Int, ans: mutable.Set[List[Int]]): Unit = {
        if (currentSum == target) {
          ans += combination.sorted.toList
          
        } else if (currentSum < target){
          val diff = target - currentSum
          (i until candidates.length).filter(idx => candidates(idx) <= diff).foreach{ case idx => dfs(idx, candidates(idx) :: combination, currentSum + candidates(idx), ans)}
        }
      }
      
      val ans = mutable.Set.empty[List[Int]]
      dfs(0, List.empty[Int], 0, ans)
      ans.toList
    }
}