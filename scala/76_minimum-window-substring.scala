/**
* chosen solution
*   time complexity: O(|S| + |T|)
*   space complexity: O(|s| + |T|)
* sliding windows: faster version
* @param
* left right : two pointer for enlarging and reducing windows size
* head and len: storing minWindow
* count: count = 0 when the range between left index and right index satisfy condition
*/
object Solution0{
    import collection.mutable
    def minWindow(s: String, t: String): String = {
      val sMap = mutable.Map.empty[Char, Int] ++ t.groupBy(identity).mapValues(_.length).toMap
      var counter = sMap.size
      
      var left = 0
      var minLength = s.length + 1
      var head = 0

      for (right <- s.indices) {
        val rightChar = s(right)
        sMap.get(rightChar) match {
          case Some(v) if v == 1 =>
            sMap.update(rightChar, v - 1)
            counter -= 1
          case Some(v) =>
            sMap.update(rightChar, v - 1)
          case None => 
        }
        
        while(counter == 0) {

          val leftChar = s(left)
          if (minLength > (right - left  + 1)) {
            head = left
            minLength = right - left + 1
          }
          
          sMap.get(leftChar) match {
            case Some(v) if v == 0 =>
              sMap.update(leftChar, v + 1)
              counter += 1
            case Some(v) =>
              sMap.update(leftChar, v + 1)
            case None =>
          }
          
          left += 1
        }
        
      }
      if (minLength == (s.length + 1)) "" else s.slice(head, head + minLength)
      
      
    }
}
/**
* my first commitment
* sliding windows with two pointer: left and right
* time complexity: O(|S| + |T|)
*/
object Solution1 {
  def minWindow(s: String, t: String): String = {

    var left = 0
    val tMap = t.groupBy(identity).mapValues(_.length).toMap

    /**
    * storing how far to reach t string's anagrams, the element's value could be negative. 
    * If negative, it means we could drop more char of this key from currentString
    */
    val budgetMap = scala.collection.mutable.Map() ++ tMap
    var currentString = ""
    var answer = ""

    for (char <- s) {
        budgetMap.get(char) match {

          case Some(e) => budgetMap.update(char, e - 1)
          case None =>
        }
      
      currentString += char

      while(!budgetMap.exists{case (_, v) => v > 0}) {
        /**
        *  drop first element from currentString if  currentString  still contains t string 
        */
        val tempChar = s(left)
        if(tMap.contains(tempChar)){
          budgetMap.update(tempChar, budgetMap.getOrElse(tempChar, 0) + 1)
        }

        if(answer.length > currentString.length || answer.isEmpty) {
          answer = currentString
        }
        currentString = currentString.drop(1)
        left += 1
      }
    }

    answer
  }
}


/**
* sliding windows : don't record string during process
*/
object Solution1-2 {
  def minWindow(s: String, t: String): String = {

    var left = 0
    var head = 0
    var len = s.length + 1

    val budgetMap = scala.collection.mutable.Map() ++ t.groupBy(identity).mapValues(_.length)

    for ((char, right) <- s.zipWithIndex) {
      
        budgetMap.get(char) match {
          case Some(e) => budgetMap.update(char, e - 1)
          case None =>
        }
      
      while(!budgetMap.exists{case (_, v) => v > 0}) {
        val tempChar = s(left)
        if(budgetMap.contains(tempChar)){
          budgetMap.put(tempChar, budgetMap(tempChar) + 1)
        }
         /* update minWindow */
        if(len > (right - left)) {
          len = right - left + 1
          head  = left
        }
        left += 1
      }
    }
    // println(budgetMap)
    if(len == (s.length + 1)) "" else s.substring(head, head + len)
  }
}

/**
* sliding windows: faster version
* left right : two pointer for enlarging and reducing windows size
* head and len: storing minWindow
* count: count = 0 when the range between left index and right index satisfy condition
*/

object Solution1-3 {
  def minWindow(s: String, t: String): String = {

    var left = 0
    var right = 0
    var head = 0
    var len = s.length + 1
    val budgetMap = scala.collection.mutable.Map() ++ t.groupBy(identity).mapValues(_.length)
    var count = budgetMap.size

    while (right < s.length) {
      val char = s(right)
     
        budgetMap.get(char) match {
            case Some(e) if e == 1 =>
            budgetMap.update(char, e - 1)
            count -= 1
            case Some(e) =>
            budgetMap.update(char, e - 1)
            case None =>
        }
      
      right += 1
      while(count == 0) {
        val tempChar = s(left)
        budgetMap.get(tempChar) match {
          case Some(e) if e == 0 =>
            budgetMap.update(tempChar, e + 1)
            count += 1
          case Some(e) =>
            budgetMap.update(tempChar, e + 1)
          case None =>
        }
        /* update minWindow*/
        if(len > (right - left)) {
          len = right - left
          head  = left
        }
        left += 1
      }
    }
    println(budgetMap)
    if(len == (s.length + 1)) "" else s.substring(head, head + len)
  }
}

/**
* 1. for loop auto increment right index
* 2. update minLength and head index before updating counter and left index
*/

object Solution1-4 {
    import collection.mutable
    def minWindow(s: String, t: String): String = {
      val sMap = mutable.Map.empty[Char, Int] ++ t.groupBy(identity).mapValues(_.length).toMap
      var counter = sMap.size
      
      var left = 0
      var minLength = s.length + 1
      var head = 0

      for (right <- s.indices) {
        val rightChar = s(right)
        sMap.get(rightChar) match {
          case Some(v) if v == 1 =>
            sMap.update(rightChar, v - 1)
            counter -= 1
          case Some(v) =>
            sMap.update(rightChar, v - 1)
          case None => 
        }
        
        while(counter == 0) {

          val leftChar = s(left)
          if (minLength > (right - left  + 1)) {
            head = left
            minLength = right - left + 1
          }
          
          sMap.get(leftChar) match {
            case Some(v) if v == 0 =>
              sMap.update(leftChar, v + 1)
              counter += 1
            case Some(v) =>
              sMap.update(leftChar, v + 1)
            case None =>
          }
          
          left += 1
        }
        
      }
      if (minLength == (s.length + 1)) "" else s.slice(head, head + minLength)
      
      
    }
}