
/**
* two pointer comparison
* memo
*  1. alphanumeric = letters + numerals
* time complexity: O(2N)
* space complexity: O(N)
*/

object Solution1 {
    def isPalindrome(s: String): Boolean = {
      val newString = s.filter(_.isLetterOrDigit).toLowerCase
      isPalindrome(newString, 0, newString.length - 1)
    }
    @annotation.tailrec
    def isPalindrome(s: String, left: Int, right: Int): Boolean = {
      if (left > right) return true
      if (s(left) == s(right)) isPalindrome(s, left + 1, right - 1)
      else false
    }
}