/**
* select solution
* time complexity: O(N KLogK) : N: strs.length, K: the longest string in strs 
*/
object Solution0 {
    def groupAnagrams(strs: Array[String]): List[List[String]] =
        strs.groupBy(_.sorted.hashCode).values.map(_.toList).toList
}

/**
* my first commit
* convert all strs into hashmap and group them by the hash value
* time complexity:  O(N K^2)
*/
object Solution1 {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    strs.groupBy(str => str.groupBy(identity).mapValues(_.length).toMap).values.map(_.toList).toList
  }
}

/**
* sort each string and groupby the sorted list's hashvalue
* time complexity: O(N KLogK) : N: strs.length, K: the longest string in strs
*/
object Solution2 {
    def groupAnagrams(strs: Array[String]): List[List[String]] =
        strs.groupBy(_.sorted.hashCode).values.map(_.toList).toList
}