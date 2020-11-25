
/**
* chosen solution
* expand around center
* time complexity: O(N * 2 * N) = O(N^2)
*        expandLengths: O(N)
* space complexity: O(1)
*/

object Solution0 {
    def longestPalindrome(s: String): String = {
        if(s == null || s.isEmpty) return ""
        
        // 0 1 2 3 4 5 6 7
        // r a c e c a r
        // r a c e e c a r
        // b b c e c a a
        val (head, maxlen) = s.indices.foldLeft((0, 1)){
            case ((h, maxlen), i) => 
                val oddlen =  expandLengths(s, i, i)
                val evenlen = expandLengths(s, i, i + 1)
                val len = oddlen max evenlen
                if(len > maxlen)  (i -  (len - 1) / 2, len)
                else (h, maxlen)
        }
        s.slice(head, head + maxlen)
    }
    // return length
    @annotation.tailrec
    def expandLengths(s: String, left: Int, right: Int): Int = {
        if(0 <= left && right < s.length && s(left) == s(right)) expandLengths(s, left - 1, right + 1)
        else right - left - 1
    }
}

/**
* my first commitment
* it's kind of brute force
* time complexity: O(N^3):
*    getPalindromeLength: O(N^2)
* space complexity: O(N)
*/

object Solution1 {
  def longestPalindrome(s: String): String = {
    /* palindromeLength(i) means  the maximum palindrome length ending at string s's index i
    *   ex:
    *     input
    *         "b a b a d"
    *          0 1 2 3 4
    *     palindromeLength(0) = 1: "b"'s max palindrome length must contains the last char => "b"
    *     palindromeLength(1) = 2: "ba"'s max palindrome length must contains the last char => "a"
    *     palindromeLength(2) = 3: "bab"'s max palindrome length must contains the last char => "bab"
    *     palindromeLength(3) = 3: "baba"'s max palindrome length must contains the last char => "baba"
    *     palindromeLength(4) = 1: "babad"'s max palindrome length must contains the last char => "babad"
    * 
    * */
    val palindromeLength = Array.ofDim[Int](s.length)
    for(right <- s.indices){

      palindromeLength(right) = getPalindromeLength(s.slice(0, right + 1))
      // println("---", right, s.slice(0, right + 1).mkString(""), cache(right))
    }
    // println(cache.mkString(","))
    val maxIdx = palindromeLength.indices.maxBy(palindromeLength)

    s.slice(maxIdx - palindromeLength(maxIdx) + 1, maxIdx + 1)

  }

  /**
    * find the letter part may contains palindrome
    * iterate left2right from 0 s.length. right2left decreases 1 if s(left2right) == s(right2left)
    * the result value of right2left is the index dividing s into two part, the latter part may contains palindrome
    * ex:
    *    input:
    *         "b a a c b a b c"
    *          0 1 2 3 4 5 6 7
    *    the splitIdx would be 2, so s[3: 7) may contains palindrome and we should recursively input s[3: 7) to check it
    */
  @annotation.tailrec
  def getPalindromeLength(s: String): Int = {
    if(s == null || s.isEmpty) return 0

    val splitIdx = s.indices.foldLeft(s.length - 1){
      case (right2left, left2right) =>
        if(s(right2left) == s(left2right)) right2left - 1
        else right2left
    }
    // println(s, splitIdx)
    if(splitIdx == -1) /* find the palindrome! */
      s.length
    else
      getPalindromeLength(s.slice(splitIdx + 1, s.length))
  }
}


/**
* brute force
* time complexity: O(N^3)
* space complexity: O(M) M is the length of longest palindrome
*/

object Solution2 {
    def longestPalindrome(s: String): String = {
        (for(i <- s.indices; j <- i until s.length) yield (i, j)).foldLeft("") {
            // pruning
            case (best, (i, j)) if best.length < (j - i + 1) && isPalindrome(s, i, j) => s.slice(i, j + 1)
            case (best, _) => best
        }

    }
    // r a c e e c a r
    // 0 1 2 3 4 5 6 7
    
    @annotation.tailrec
    def isPalindrome(s: String, l: Int, r: Int): Boolean = {
    
        if(s == null || s.isEmpty) false
        else if(l >= r) true
        else { // l < r
            // println(l, s(l), r, s(r))
            if(s(l) != s(r)) false
            else isPalindrome(s, l + 1, r - 1)
        }
        
    } 
}

/**
* dynamic programming
* state definition
*     dp(i)(j) represents wether substring s(i: j) is palindromic
*     ex: 
*       s: "r a c e c a r"
*           0 1 2 3 4 5 6
*       dp(1)(5) is true due to "a c e c a" is palindrome
* state transformation
*      1.dp(i)(j) = (s(i) == s(j)) && dp(i + 1)(j - 1) if  (j - i) - (i + 1) + 1 < 2 
*         due to dp(i + 1)(j - 1) exceeds the edge 
*         ex:
*         s: "l e e t c o d e"
*             0 1 2 3 4 5 6 7
*         let i = 3, j = 4 => i + 1 = 4, j - 1 = 3 => dp(i + 1)(j - 1) = dp(4)(3) => it doesn't make sense
*   
* time complexity: O(N^2)
* space complexity: O(N^2)
*/
object Solution3 {
    def longestPalindrome(s: String): String = {
        if(s == null || s.isEmpty ) return ""
        if(s.length < 2) return s
        
        /**
        * if we initial the dp table dp(i)(j) with iterating all of element, it's time consuming
        */
        // val dp = Array.tabulate(s.length, s.length){
        //     case (i, j) if i == j => true
        //     case _ => false
        // }
        val dp = Array.ofDim[Boolean](s.length, s.length)
        var maxLen = 1
        var head = 0
        /** dp(i)(j) = (s(i) == s(j)) && dp(i + 1)(j - 1)
        * dp(i)(j)  depends on dp(i + 1)(j - 1), so we need calculate dp(i + 1)(j - 1) before we calculate dp(i)(j)
        *    0 1 2 3 4 5 6
        *          j
        *   0  A B D G K P
        *   1    C E H L Q
        *   2      F I M R
        * i 3        J N S
        *   4          O T
        *   5            U 
        *   6
        *  the iterative order would be 
        *     * A -> B -> C -> D -> E -> F .... => (0, 1) -> (0, 2) -> (1, 2) -> (0, 3) -> (1, 3) -> (2, 3) ... and so on
        *      
        *     
        */
        for(j <- 1 until s.length; i <- 0 until j){
            val currentLen = j - i + 1
            if(s(i) != s(j))  dp(i)(j) = false
            else if(currentLen < 4)  dp(i)(j) = true // currentLen - 2 < 2
            else dp(i)(j) = dp(i + 1)(j - 1)
            
            
            if(dp(i)(j) && currentLen > maxLen){
                maxLen = currentLen
                head = i

            }
        }
        
        s.slice(head, head + maxLen)
    }
}


/**
* expand around center
* time complexity: O(N^2)
*        expandLengths: O(N)
* space complexity: O(1)
*/

object Solution4 {
    def longestPalindrome(s: String): String = {
        if(s == null || s.isEmpty) return ""
        
        // 0 1 2 3 4 5 6 7
        // r a c e c a r
        // r a c e e c a r
        // b b c e c a a

        // var head = 0
        // var maxlen = 1
        // for(i <- s.indices) {
        //     val oddlen =  expand(s, i, i)
        //     val evenlen = expand(s, i, i + 1)
        //     val len = oddlen max evenlen
        //     if(len > maxlen){
        //         head = i -  (len - 1) / 2
        //         maxlen = len
        //     }
        // }
        val (head, maxlen) = s.indices.foldLeft((0, 1)){
            case ((h, maxlen), i) => 
                val oddlen =  expandLengths(s, i, i)
                val evenlen = expandLengths(s, i, i + 1)
                val len = oddlen max evenlen
                if(len > maxlen)  (i - (len - 1) / 2, len)
                else (h, maxlen)
        }
        s.slice(head, head + maxlen)
    }
    // return length
    @annotation.tailrec
    def expandLengths(s: String, left: Int, right: Int): Int = {
        if(0 <= left && right < s.length && s(left) == s(right)) expandLengths(s, left - 1, right + 1)
        else right - left - 1
    }
}
