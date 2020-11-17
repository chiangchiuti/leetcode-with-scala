/**
* dynamic programming  - Levenshtein distance
* memo
*    1. dp(i)(j) represent the minimum edit distance from the length i substring from word1 to the length j substring from word2
*    2. dp(i)(j) is solved by its sub-optimal problem 
*         1, delete op: dp(i -1)(j)
*         2. replacement op: dp(i -1)(j - 1)
*         3. insertion op: dp(i)(j - 1)
* time complexity: O(NM) N is the length of word1, N is the length of word2
* space complexity: O(NM)
*/
object Solution1 {
  def minDistance(word1: String, word2: String): Int = {
    val m = word1.length
    val n = word2.length
    /* initial  Levenshtein distance table 
    * dp(i)(j) represent the minimum distance transforming from length i of substring word1 to length j of substring word2
    */
    val dp = Array.tabulate(m + 1, n + 1) {
      case (0, j) => j
      case (i, 0) => i
      case _ => 0
    }

    for (i <- 1 to m; j <- 1 to n) {
      /* i-1 is word1 index, j-1 is word2 index */
      if (word1(i - 1) == word2(j - 1)) {
        // do nothing case
        dp(i)(j) = dp(i - 1)(j - 1)
      } else {
        /**
        *       i-1,    i
        * j-1 replace  insertion     
        *  j   delete  dp(i)(j)
        */
        val replace = dp(i - 1)(j - 1)
        val insert = dp(i)(j - 1)
        val delete = dp(i - 1)(j)
        dp(i)(j) = (replace min insert min delete) + 1
      }
    }
    dp(m)(n)
  }
}