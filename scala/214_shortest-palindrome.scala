/**
* my first commitment
*   1. pointer to find the middle index which can pair all front part
*   2. iterate char from the middle index to end of string generating the  shortest palindrome
*        should judge the odd or even case
*               odd:   abcba  => middle index is 2
*               even： abccba => middle index is 3             
*
* time complexity O(N * M) M is the longest pair size
*
* memo:
*    odd and even judgement
*/
object Solution1 {
  def shortestPalindrome(s: String): String = {
    if(s == null || s.isEmpty) return ""
    var pointer = 0
    var length = 0
    var middle = 0
    while(pointer < s.length) {
      var left = pointer - 1
      var right = if(judgeOdd(s, pointer)) pointer + 1 else pointer
      while(left >= 0 && right < s.length && s(left) == s(right)){
        if(left == 0 && right - pointer >= length){
          middle = pointer
          length = right - middle + 1
        }
        right += 1
        left -= 1
      }
      pointer += 1
    }
    generate(s, middle)
  }

  private def judgeOdd(s: String, idx: Int): Boolean = {
    val identityCounter = 1 + (idx + 1 until s.length).takeWhile(right => s(idx) == s(right)).length + (idx - 1  to 0 by -1).takeWhile(left => s(idx) == s(left)).length
    identityCounter % 2 != 0
  }
  private def generate(s: String, middle: Int): String = {
    val odd = judgeOdd(s, middle)

    (middle + 1 until s.length).foldLeft(if(odd) s(middle).toString else  s(middle).toString * 2){
      case (str, idx ) => s(idx) + str + s(idx)
    }
  }
}


/**
*  pointer recursive version
*  MEMO:
*    1. using pointer to find the char index cannot form palindrome arbitrarily in a string
*    2. the palindrome part must be contained in the former part            
*/

object Solution2 {
    def shortestPalindrome(s: String): String = {
        if(s == null || s.isEmpty) return ""
        var split = 0

        (s.length - 1 to 0 by -1).foreach{ case i => if(s(i) == s(split)) split += 1}
        val latterPart = s.slice(split, s.length)
        if(split == s.length) 
            s
        else
            latterPart.reverse + shortestPalindrome(s.slice(0, split)) + latterPart
    }
}

/**
* improve by function programming
*/
object Solution2-1 {
    def shortestPalindrome(s: String): String = {
        if(s == null || s.isEmpty) return ""
        
        val splitIdx = (s.indices.reverse).foldLeft(0){
            case (head2TailIdx, tail2HeadIdx) =>
                if(s(head2TailIdx) == s(tail2HeadIdx)) head2TailIdx + 1
                else head2TailIdx            
        }
        if(splitIdx == s.length) 
            s
        else
            s.slice(splitIdx, s.length).reverse + shortestPalindrome(s.slice(0, splitIdx)) + s.slice(splitIdx, s.length)
    
    }
}
// 


/**
* brute force
*   THE intuition is find the longest palindrome for the head of string
*   1. find the stripIdx which make the s[0: idx] = reverse(s[0: idx])
*   2. the final result is: reverse(s[idx + 1: ]) + s
*        ex : take abcbabcab for example： 
*            idx = 4 which divide the string into two part abcba and bcab
*            then, the final string is reverse(bcab) + abcbabcab = bacb + abcbabcab = bacbab c babcab
*   time complexity: O(N^2) N is the string length
*/
object Solution3 {
    def shortestPalindrome(s: String): String = {
        val n = s.length       
        val stripIdx = (s.indices.reverse).find{ idx => 
            // println(left, right, s.slice(0, idx), s.slice(0,  idx).reverse)            
             (0 until idx).lazyZip(idx to 0 by -1).forall{ 
                case (l, r) => 
                    // println(s"$l: ${s(l)},  $r: ${s(r)}")
                    s(l) == s(r)
            }
        }.getOrElse(0)
        s.slice(stripIdx + 1, n).reverse + s
    }

}
