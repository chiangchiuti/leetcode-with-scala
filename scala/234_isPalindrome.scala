/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
 
 /**
 * my first commitment
 *  using two pointer, one run 2 times faster than the other
 */
object Solution1 {
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
            /**
                1 2 2 1 null
                s f
                    s   f  

                1 2 3 2 1 null
                s f   
                    s   f
            */
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


/**
* very brilliant solution
*/
object Solution2 {
    def isPalindrome(head: ListNode): Boolean = {
        if (head == null) {
            return true
        }
        var p = head
        var result = true
        def go(node: ListNode): Unit = {
            if (node.next != null) {
                go(node.next)
            }
            if (p.x != node.x) {
                result = false
            }
            p = p.next
        }
        go(head)
        result
    }
}