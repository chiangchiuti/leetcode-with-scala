/**
* select solution
*   time complexity: O(|S| + |T|)
*   space complexity: O(|T|)
* sliding windows: faster version
* @param
* left right : two pointer for enlarging and reduce windows size
* head and len: storing minWindow
* count: count = 0 when  "left index until right index"  satisfy condition
*/

object Solution0 {
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
                // char count would be zero
                budgetMap.update(char, e - 1)
                count -= 1
            case Some(e) =>
                budgetMap.update(char, e - 1)
            case None =>
        }
      
      right += 1
      /* while  condition means current window contains t */
      while(count == 0) {
      /* update minWindow*/
        if(len > (right - left)) {
          len = right - left
          head  = left
        }

        val tempChar = s(left)
        budgetMap.get(tempChar) match {
          case Some(e) if e == 0 =>
            budgetMap.update(tempChar, e + 1)
            count += 1
          case Some(e) =>
            budgetMap.update(tempChar, e + 1)
          case None =>
        }
        
        left += 1
      }
    }

    if(len == (s.length + 1)) "" else s.substring(head, head + len)
  }
}

/**
* my first commit
* sliding windows
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
* left right : two pointer for enlarging and reduce windows size
* head and len: storing minWindow
* count: count = 0 when  left index until right index  satisfy condition
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