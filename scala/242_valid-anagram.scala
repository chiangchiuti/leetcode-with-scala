
/**
* O(N)
*/

object Solution {
  def isAnagram(s: String, t: String): Boolean = {
    charCounter(s) equals charCounter(t)
  }
  private def charCounter(str: String): Map[Char, Int] = {
    str.foldLeft(collection.mutable.Map.empty[Char, Int]) {
      (map, s) =>
        map.get(s) match {
          case Some(e) =>
            map.update(s, e + 1)
            map
          case None =>
            map.update(s, 1)
            map
        }
    }.toMap
  }
}


/**
*  one line version
*/

object Solution {
  def isAnagram(s: String, t: String): Boolean = {
   s.groupBy(identity).mapValues(_.length).toMap == t.groupBy(identity).mapValues(_.length).toMap

  }
}