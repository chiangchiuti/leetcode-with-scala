object Solution {
  def minDistance(word1: String, word2: String): Int = {
    val m = word1.length
    val n = word2.length
    val dp = Array.tabulate(m + 1, n + 1) {
      case (0, j) => j
      case (i, 0) => i
      case _ => 0
    }

    for (i <- 1 to m; j <- 1 to n) {

      if (word1(i - 1) == word2(j - 1)) {
        dp(i)(j) = dp(i - 1)(j - 1)
      } else {

        val replace = dp(i - 1)(j - 1)
        val insert = dp(i)(j - 1)
        val delete = dp(i - 1)(j)
        dp(i)(j) = (replace min insert min delete) + 1
        
        // int -> exet: do add on left side base on  int -> exe
        // int -> exet: do delete on right side base on in -> exet
      }
    }
    dp(m)(n)
  }
}