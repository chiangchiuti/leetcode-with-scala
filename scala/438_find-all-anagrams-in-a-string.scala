/**
* chosen solution
* sliding window - two pointer template version
* two index to indicate range: left and right
* time complexity: O(N)
* space timeComplexity: O(N) : one hashMap
*/
object Solution0 {
  import scala.collection.mutable
  def findAnagrams(s: String, p: String): List[Int] = {
    val pMap = mutable.Map.empty ++ p.groupBy(identity).mapValues(_.length).toMap
    val ret = scala.collection.mutable.ListBuffer[Int]()
    var left = 0
    var counter = pMap.size

    for(right <- s.indices) {  // right index

      pMap.get(s(right)) match {
        case Some(e) if e >= 1 =>  // e >=1 means the char exits in p
          pMap.update(s(right),  e - 1)
          counter -= 1  // match a char
        case Some(e) =>  // e <= 0 meas there would be duplicate char in s but p isn't
          pMap.update(s(right),  e - 1)
        case None =>
      }
      while(counter == 0) {
        if((right - left + 1) == p.length) ret += left
        pMap.get(leftChar) match {
            case Some(v) =>
                pMap.update(leftChar, v + 1)
                if(v == 0) counter += 1
            case None =>
        }
        left += 1
      }
    }
    ret.toList
  }
}

/**
* my first commit
* sliding window + hashMap within windows
* time complexity: O(NM): N: s.length, M: p.length
*/

object Solution1 {
  def findAnagrams(s: String, p: String): List[Int] = {
    val pMap = p.groupBy(identity).mapValues(_.length).toMap

     s.sliding(p.length).zipWithIndex.filter{ case (c, _) => pMap == c.groupBy(identity).mapValues(_.length).toMap}.map(_._2).toList
  }
}


/**
* sliding window - two pointer template version
* two index to indicate range: left and right
* time complexity: O(N)
* space timeComplexity: O(N) : one hashMap
*/

object Solution2 {
  import scala.collection.mutable
  def findAnagrams(s: String, p: String): List[Int] = {
    val pMap = mutable.Map.empty ++ p.groupBy(identity).mapValues(_.length).toMap
    val ret = scala.collection.mutable.ListBuffer[Int]()
    var left = 0
    var counter = pMap.size

    for(right <- s.indices) {  // right index

      pMap.get(s(right)) match {
        case Some(e) if e >= 1 =>  // e >=1 means the char exits in p
          pMap.update(s(right),  e - 1)
          counter -= 1  // match a char
        case Some(e) =>  // e <= 0 meas there would be duplicate char in s but p isn't
          pMap.update(s(right),  e - 1)
        case None =>
      }
      while(counter == 0) {
        if((right - left + 1) == p.length) ret += left
        pMap.get(leftChar) match {
            case Some(v) =>
                pMap.update(leftChar, v + 1)
                if(v == 0) counter += 1
            case None =>
        }
        left += 1
      }
    }

    ret.toList
  }
}

/**
* sliding window - two hashmap version
* using a mutable map storing current window's string element and amount
* time complexity: O(N)
* space complexity: O(2N) -> two hashMap
*/

object Solution2-1 {
  def findAnagrams(s: String, p: String): List[Int] = {
    val pMap = p.groupBy(identity).mapValues(_.length).toMap
    val sMap = scala.collection.mutable.Map[Char, Int]()
    val result = scala.collection.mutable.ListBuffer[Int]()

    for((char, right) <- s.zipWithIndex) {
      sMap.put(char, sMap.getOrElse(char, 0) + 1)

      if(right >= p.length) {
        val leftChar = s(right - p.length)
        sMap.get(leftChar) match {
          case Some(e) if e == 1 => sMap.remove(leftChar)
          case Some(e) => sMap.update(leftChar, e - 1)
          case _ =>
        }
      }
      if(pMap.equals(sMap)) result += (right - p.length + 1)
    }
    result.toList
  }
}
