


/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */

 /**
 * my first commitment
 * time complexity O(max(l1.length, l2.length))
 */
object Solution1 {
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
      val dummyHead = ListNode()
      var current = dummyHead
      var (p1, p2) = (l1, l2)
      var carry = 0
      while(p1 != null && p2 != null) {
        val sum = carry + p1.x + p2.x
        carry = sum / 10
        
        current.next = ListNode(sum % 10)
        current = current.next
        p1 = p1.next
        p2 = p2.next
      }
      
      while(p1 != null) {
        val sum = carry + p1.x
        carry = sum / 10
        current.next = ListNode(sum % 10)  
        current = current.next
        p1 = p1.next
      }
      while(p2 != null) {
        val sum = carry + p2.x
        carry = sum / 10
        current.next = ListNode(sum % 10)
        current = current.next
        p2 = p2.next
      }
      if (carry > 0)
        current.next = ListNode(carry)
      dummyHead.next
    }
}