

/**
* my first commitment: sliding window: hashcode with build-in sliding method
*/
object Solution1 {
    def checkInclusion(s1: String, s2: String): Boolean = {
      val s1Length = s1.length
      val s1Hash = s1.groupBy(identity).mapValues(_.length).toMap.hashCode

      s2.sliding(s1Length).exists(seq => seq.groupBy(identity).mapValues(_.length).toMap.hashCode == s1Hash)
    }
}


/**
* optimize from 1-1 : maintain sliding window and hashmap by my self
* time complexity: O(s1.length + s2.length)
* space complexity: O(s1.distinct.length)
*/
object Solution1-2 {
    import collection.mutable
    def checkInclusion(s1: String, s2: String): Boolean = {
      val s1HashCode = s1.groupBy(identity).mapValues(_.length).toMap.hashCode
      val s2Map = mutable.Map.empty[Char, Int]
      
      (0 until s2.length).exists {
        case idx if idx < s1.length =>
          val char = s2(idx)
          mapIncrement(s2Map, char)
          s2Map.hashCode == s1HashCode
        case idx => 
          val rightChar = s2(idx)
          val leftChar = s2(idx - s1.length)
          mapIncrement(s2Map, rightChar)
          mapDecrement(s2Map, leftChar) 
          s2Map.hashCode == s1HashCode
      }
    }
    def mapIncrement(map: mutable.Map[Char, Int], char: Char): Unit = {
      map.get(char) match {
            case Some(v) => map.update(char, v + 1)
            case None => map.update(char, 1)
      }
    }
    def mapDecrement(map: mutable.Map[Char, Int], char: Char) = map.get(char) match {
      case Some(v) if v == 1 => map.remove(char)
      case Some(v) => map.update(char, v - 1)
      case None =>
    }
}

/**
* sliding windows: using only 1 map to record differential
* 1. if diff map is empty, s2 contains the permutation of s1
* 2. initial map with foreach instead of groupBy witch is time consuming
* time complexity (l1 + l2)
*/
object Solution1-3 {
    import collection.mutable
    def checkInclusion(s1: String, s2: String): Boolean = {
      val diffMap = mutable.Map.empty[Char, Int]
      s1.foreach(mapUpdate(diffMap, _, 1))
      
      (0 until s2.length).exists {case idx =>
        if (idx >= s1.length) {
          val leftChar = s2(idx - s1.length)
          mapUpdate(diffMap, leftChar, 1)
        }
        val rightChar = s2(idx)
        mapUpdate(diffMap, rightChar, -1)
        diffMap.isEmpty
      }
    }
    def mapUpdate(map: mutable.Map[Char, Int], char: Char, value: Int): Unit = {
      map.get(char) match {
            case Some(v) if v + value == 0 => map.remove(char)
            case Some(v) => map.update(char, v + value)
            case None => map.update(char, value)
      }
    }
}