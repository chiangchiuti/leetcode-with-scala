/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
    def reverseList(head: ListNode): ListNode = {
        
        var prev: ListNode = null
        var curr = head

        while (curr != null) {
            val hold = curr.next
            curr.next = prev
            prev = curr
            curr = hold
        }
        // printNode(curr)
        prev
    }
    
    def printNode(node: ListNode) {
        var n = node
        while(n != null) {
            print(s"${n.x} ")
            n = n.next
        }
    }
}