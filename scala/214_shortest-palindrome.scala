/**
* chosen solution
*  memo
*     1. find the largest palindrome segment from beginning ,  and reverse remaining substring plus original string
*     2. expand around center method
* time complexity: O(N * 2N) = O(N^2)
*/
object Solution0 {
    def shortestPalindrome(s: String): String = {
        if(s == null || s.isEmpty) return ""
       
        val splitIdx = s.indices.foldLeft(0){
            case(splitIdx, i) =>
                val oddlen = expandLength(s, i, i)
                val evenlen = expandLength(s, i, i + 1)
                val len = oddlen max evenlen
                val h = i - (len - 1) / 2
                // splitidx is equal to maxlen starting from zero
                val previousMaxlen = splitIdx
                if(h == 0 && len > previousMaxlen) len
                else splitIdx
        }
        s.slice(splitIdx, s.length).reverse + s
    }
    @annotation.tailrec    
    def expandLength(s: String, left: Int, right: Int): Int = {
        if(0 <= left && right < s.length && s(left) == s(right)) expandLength(s, left - 1, right + 1)
        else right - left - 1       
    }
}
/**
* my first commitment - kind of brute force
*   1. pointer to find the middle index which can pair all front part .aka. find the index which can split string s into two part
*   2. iterate char from the middle index to end of string to generate the shortest palindrome
*        should judge the odd or even case
*               odd:   abcba  => middle index is 2
*               even： abccba => middle index is 3             
*
* time complexity O(N * N)
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
* simplify by expanding around center method
* time complexity: O(N * 2N) - O(N^2)
*/
object Solution1-2 {
    def shortestPalindrome(s: String): String = {
        if(s == null || s.isEmpty) return ""
       
        val splitIdx = s.indices.foldLeft(0){
            case(splitIdx, i) =>
                val oddlen = expandLength(s, i, i)
                val evenlen = expandLength(s, i, i + 1)
                val len = oddlen max evenlen
                val h = i - (len - 1) / 2
                // splitidx is equal to maxlen counting from zero
                val previousMaxlen = splitIdx
                if(h == 0 && len > previousMaxlen) len
                else splitIdx
        }
        s.slice(splitIdx, s.length).reverse + s
    }
    @annotation.tailrec    
    def expandLength(s: String, left: Int, right: Int): Int = {
        if(0 <= left && right < s.length && s(left) == s(right)) expandLength(s, left - 1, right + 1)
        else right - left - 1       
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
*   THE intuition is finding the longest palindrome s[0: idx] counting from the beginning of the string 
*   1. find the largest stripIdx which make the s[0: idx] = reverse(s[0: idx])
*          ex: input  abcbagg
*                abcbagg, ggabcba
*                abcbag, gabcba
*                abcba, abcba   => longest palindrome start from index 0
*                abcb, bcba
*                abc, cba
*                ab, ba
*                a, a
*    
*   2. the final result is: reverse(s[idx + 1: ]) + s
*        ex : take abcbabcab for example： 
*            idx = 4 means dividing the string into two part abcba and bcab
*            ,then the final string is reverse(bcab) + abcbabcab = bacb + abcbabcab = bacbab c babcab
*   time complexity: O(N^2) N is the string length
*/
object Solution3-1 {
    def shortestPalindrome(s: String): String = {
        
        val reverseS = s.reverse
        for(i <- reverseS.indices){
            if(reverseS.slice(i, reverseS.length) == s.slice(0, s.length - i)) {
                return reverseS.slice(0, i) + s
            }
        }
        "" 
    }
}

/**
* brute force: compare by char by char
* performance is worst than above one
*/
object Solution3-2 {
    def shortestPalindrome(s: String): String = {
        val n = s.length       
        val stripIdx = (s.indices.reverse).find{ idx => 
            // println(left, right, s.slice(0, idx), s.slice(0,  idx).reverse)            
             /* generate forward and backward index pair  */
             (0 until idx).lazyZip(idx to 0 by -1).forall{ 
                case (l, r) => 
                    // println(s"$l: ${s(l)},  $r: ${s(r)}")
                    s(l) == s(r)
            }
        }.getOrElse(0)
        s.slice(stripIdx + 1, n).reverse + s
    }

}


/**
* KMP - accelerate by next or PMT table
* memo
*    1. improving from brute force
*    2. the problem is finding the longest palindrome substring starts from index 0
*       in brute force method, we compare s(0: idx) with reversedS(n - idx: n) to find out the longest palindrome
*        ex: s: "abcbagg", reversed s: "ggabcba"
*           a, a
*           ab, ba
*           abc, cba
*           abcb, bcba
*           abcba = abcba  => longest palindrome
*           abcbag, gabcba
*       that is, we are looking for the longest prefix A from s and longest suffix B from reversed s which make A is equal to B
*               => longestPrefix(abcbagg) = longestSuffix(ggabcba)
*       So, the ideas is we could create a next table as what we do in KMP alg
*       There is a trick to build a temp string like this: s + '#' + reverse(s)
*       and then create a next table from the temp string:
*         index:       0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
*         tempString   a b c b a g g # g g a  b  c  b  a
*         pmt(next):   0 0 0 0 1 0 0 0 0 0 1  2  3  4  5
*       see?  pmt[14] is 5 which means the longest prefix length is 5 => a b c b a
*       and we have found the longest palindrome in the s starting from index 0.
* 
* time complexity: O(N)
*            generatePMT: O(N)
* space complexity: O(N)
*/

object Solution4 {
    def shortestPalindrome(s: String): String = {

        val newString = s + '#' + s.reverse
        val pmt = generatePMT(newString)
        s.slice(pmt.last, s.length).reverse + s
    }
    
    def generatePMT(s: String): Array[Int] = {
        val pmt = Array.ofDim[Int](s.length)
        pmt(0) = 0
        var prefixIdx = 0
        for(i <- 1 until s.length) {
            while(prefixIdx > 0 && s(i) != s(prefixIdx)) {
                prefixIdx = pmt(prefixIdx - 1)
            }
            
            if(s(i) == s(prefixIdx)) {
                prefixIdx += 1
                pmt(i) = prefixIdx
            } else pmt(i) = 0
        }
        pmt
    }
}