/**
* chosen solution
* time complexity: O(N KLogK) : N: strs.length, K: the longest string in strs 
*/
object Solution0 {
    def groupAnagrams(strs: Array[String]): List[List[String]] =
        strs.groupBy(_.sorted.hashCode).values.map(_.toList).toList
}

/**
* my first commit
* convert all strs into hashmap and group them by the hash value
* time complexity:  O(N K) , but groupBy op is slower
*/
object Solution1 {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    strs.groupBy(str => str.groupBy(identity).mapValues(_.length).toMap).values.map(_.toList).toList
  }
}

/**
* inner groupBy is hands-on
* memo:
*   1. categorize by count
*/
object Solution1-2{
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
        strs.toList.groupBy{str => 
            val hashmap = scala.collection.mutable.Map.empty[Char, Int]
            str.foreach(char => hashmap.update(char, hashmap.getOrElse(char, 0) + 1))
            hashmap.hashCode
        }.values.toList
        
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

