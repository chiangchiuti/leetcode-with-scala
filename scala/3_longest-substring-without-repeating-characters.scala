/**
* chosen solution
* two pointer to control sliding window
*   1. two pointer: left and right to control substring window
*   2. counter and hashmap to record whether current window is valid or not
* time  complexity: O(N), worst: O(2N) -> each char was visited twice
*/

object Solution0 {
  def lengthOfLongestSubstring(s: String): Int = {
    val sMap = scala.collection.mutable.Map[Char, Int]() ++ s.distinct.map(c => (c, 0)).toMap
    var left = 0
    var right = 0
    var counter = 0
    var length = 0
    while (right < s.length) {
      val rightChar = s(right)
      sMap.get(rightChar) match {
        case Some(v) if v >= 1 =>
          sMap.update(rightChar, v + 1)
          counter += 1
        case Some(v) =>
          sMap.update(rightChar, v + 1)
      }
      right += 1
      while (counter > 0) {
        val leftChar = s(left)
        sMap.get(leftChar) match {
          case Some(v) if v > 1 =>
            sMap.update(leftChar, v - 1)
            counter -= 1
          case Some(v) =>
            sMap.update(leftChar, v - 1)
        }

        left += 1

      }
      length = length max (right - left)
    }
    length
  }
}


/**
* my first commit
* sliding windows
*  time  complexity: O(N), worst: O(2N) -> each char was visited twice
*/
object Solution1 {
    def lengthOfLongestSubstring(s: String): Int = {
        var right = 0
        var left = 0
        var current = ""
        var ret = ""
        
        while(right < s.length) {
            val char = s(right)
            if (current.contains(char)){
                current = current.drop(1)
                left += 1
                 
            }else {
                right += 1
                current += char
            }     
            if(current.length > ret.length) ret = current
        }
        ret.length
    }
}


/**
* sliding windows, slower than solution1
*   memo
*     1. using hashmap to record whether the current right char is duplicated or not
*/
object Solution1-2 {
    def lengthOfLongestSubstring(s: String): Int = {
        val map = scala.collection.mutable.Map[Char, Int]() ++ s.distinct.map(c => (c, 0))
        var left = 0
        var right = 0
        var length = 0
        
        while(right < s.length){
            val rightChar = s(right)
        
            map.update(rightChar, map(rightChar) + 1)
            right += 1
            
            /* iterate until meet condition */
            while(map(rightChar) > 1){
                val leftChar = s(left)
                
                map.get(leftChar) match {
                    case Some(v) if v > 0 =>  map.update(leftChar,  v - 1)
                    case _ =>
                }
                
                left += 1
            }

            length = length max (right - left)  // update minimum
               
        }
        length
    }
}

/**
* using substring problem template
*   1. two pointer: left and right to control substring window
*   2. counter and hashmap to record whether current window is valid or not
*/
object Solution1-3 {
  def lengthOfLongestSubstring(s: String): Int = {
    val sMap = scala.collection.mutable.Map[Char, Int]() ++ s.distinct.map(c => (c, 0)).toMap
    var left = 0
    var right = 0
    var counter = 0
    var length = 0
    while (right < s.length) {
      val rightChar = s(right)
      sMap.get(rightChar) match {
        case Some(v) if v >= 1 =>
          sMap.update(rightChar, v + 1)
          counter += 1
        case Some(v) =>
          sMap.update(rightChar, v + 1)
      }
      right += 1
      while (counter > 0) {
        val leftChar = s(left)
        sMap.get(leftChar) match {
          case Some(v) if v > 1 =>
            sMap.update(leftChar, v - 1)
            counter -= 1
          case Some(v) =>
            sMap.update(leftChar, v - 1)
        }

        left += 1

      }
      length = length max (right - left)
    }
    length
  }
}

