/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
    def isPalindrome(head: ListNode): Boolean = {
        if (head == null){
            true
        }else {
            var slowPre: ListNode = null
            var slow = head
            var fast = head

            while (fast != null && fast.next != null) {
                fast = fast.next.next

                val slowNext = slow.next
                slow.next = slowPre
                slowPre = slow
                slow = slowNext

            }

            fast match {
                case null => checkPalindrome(slowPre, slow)
                case _ => checkPalindrome(slowPre, slow.next)  // odd case
            }
        }
       
                
    }
    def checkPalindrome(left: ListNode, right: ListNode): Boolean = {
        (left, right) match {
            case (null, null) => true
            case (l, r) if l != null && r != null && l.x == r.x => checkPalindrome(left.next, right.next)
            case _ => false   
        }
        
    }
    

    def printNode(node: ListNode) {
        var n = node
        
        while(n != null) {
            print(s"${n.x}\t")
            n = n.next
        }
    }
}