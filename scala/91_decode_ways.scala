

/**
* my first commitment dynamic programming
* memo:
* 1.subproblem dp(i) represents the decode ways of the sub-string which length is i 
* 2. dp(i) could be sum from dp(i-1) or dp(i-2) if s(i -1, i) or s(i-2, i) are valid coding
*  idx:   0 1 2 3 4 5 6 7
*  length 1 2 3 4 5 6 7 8
&  value  1 2 1 3 2 5 8 3
*   
*   dp(1) => "1"
*   dp(2) => "12" :
*            valid("12") + dp(0)
*            valid("2") + dp(1)
*   dp(3) => "121" :
*           valid("21") + dp(1)
*           valid("1) + dp(2)
*
* time complexity: O(2N)
* space complexity: O(N)
*/

object Solution1 {
    def numDecodings(s: String): Int = {
      if(s == null || s.length == 0) return 0 
      val dp = Array.ofDim[Int](s.length + 1)
      dp(0) = 1
      dp(1) = if (s(0) == '0') 0 else 1
      (2 to s.length).foreach { idx =>
        val single = s.slice(idx-1, idx).toInt
        val tens = s.slice(idx-2, idx).toInt
        if (0 < single && single <= 9)
          dp(idx) += dp(idx-1)
        if (10 <= tens && tens <= 26)
          dp(idx) += dp(idx-2)
      
      }
      dp.last
    }
}
/**
* instead of using slice, handle single and tens by hands
* memo
*  1. char as digit
*/

object Solution1-1 {
    def numDecodings(s: String): Int = {
      if(s == null || s.length == 0) return 0 
      val dp = Array.ofDim[Int](s.length + 1)
      dp(0) = 1
      dp(1) = if (s(0) == '0') 0 else 1
      (2 to s.length).foreach { idx =>
        val single = s(idx-1).asDigit
        val tens = s(idx-2).asDigit * 10 + single
        if (0 < single && single <= 9)
          dp(idx) += dp(idx-1)
        if (10 <= tens && tens <= 26)
          dp(idx) += dp(idx-2)
      }
      
      dp.last
      
    }
}

/**
*  dp - only keep dp(i-1) and dp(i-2)
* time complexity: O(2N)
* space complexity: O(1)
*/

object Solution1-3 {
    def numDecodings(s: String): Int = {
      if (s == null && s.isEmpty) return 0
      (2 to s.length).foldLeft((1, if(s(0) == '0') 0 else 1)) {
      /**
      * pre = dp(i-1)
      * prepre = dp(i-2) 
      */
        case ((prepre, pre), idx) =>
          val decodeOne = if(decodeSingle(s, idx)) pre else 0
          val decodeTwo = if(decodeTens(s, idx)) prepre else 0
          (pre, decodeOne + decodeTwo)
      }._2
    }
  
    def decodeSingle(s: String, idx: Int): Boolean = s(idx - 1) != '0'
  
    def decodeTens(s: String, idx: Int): Boolean = (s(idx - 2) == '1' ) || (s(idx - 2) == '2' && s(idx-1) <= '6' )
    
}


