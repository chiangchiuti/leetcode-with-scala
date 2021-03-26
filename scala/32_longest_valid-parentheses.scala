


/**
* using stack to record the char index in oder to calculate the valid length
* memo:
* 1. always only have one invalid symbol at stack and its position index is 0
* time complexity O(n)
* space complexity O(n)
*/
object Solution1 {

  import collection.mutable

  def longestValidParentheses(s: String): Int = {
    val mapping = Map('(' -> ')')
    val stack = mutable.Stack[Int]()
    stack.push(-1)
    s.indices.foldLeft(0) {
      case (maxLength, idx) =>
        val char = s(idx)
        if (mapping.contains(char)) {
          stack push idx
          maxLength
        } else {
          stack.pop()
          if (stack.isEmpty) {
            stack push idx
            maxLength
          } else {
            (idx - stack.head) max maxLength
          }
        }
    }
  }
}