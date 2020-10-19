/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution1 {
    def swapPairs(head: ListNode): ListNode = {
        val nHead = new ListNode(0, head)
        var pre: ListNode  = nHead
        var curr = pre.next

        while (curr != null && curr.next != null) {
            val (pos1, pos2, next) = (curr, curr.next, curr.next.next)
            pre.next = pos2
            pos2.next = pos1
            pos1.next = next

            pre = pre.next.next
            curr = pre.next  
        }
        
        nHead.next
    }
}



object Solution2 {
    def swapPairs(head: ListNode): ListNode = {
        _swap(head)
    }   
    
    def _swap(n: ListNode): ListNode = {
        if(n == null) n
        else {
            (n, n.next) match {
                case (a, null) => a
                case (a: ListNode, b: ListNode) => 
                    /** a b 要交換位子
                     */
                    a.next = _swap(b.next) // a 指向 b 的 next (已交換完成）
                    b.next = a // b 的 next 接上 a 就交換完成
                    b
            }
        } 
    }
}