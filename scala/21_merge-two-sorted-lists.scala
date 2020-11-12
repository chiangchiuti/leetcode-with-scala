/**
* select solution
* time complexity: O(N + M), N is the length of l1, M is the length of l2
*/

object Solution0 {
    def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
        val headNode = new ListNode(-1, null)
        var cur = headNode
        
        var no1 = l1;
        var no2 = l2;
        
        while(no1 != null && no2 != null) {
            if (no1.x >= no2.x){
                
                cur.next = no2
                no2 = no2.next
            }else {
                cur.next = no1
                no1 = no1.next
            }
            cur = cur.next
        }
        (no1, no2) match {
            case (_, null) => cur.next = no1
            case (null, _) => cur.next = no2
            case _ => throw new RuntimeException()
        }
        
        headNode.next
    }
}



/**
* iterative version
* time complexity: O(N + M), N is the length of l1, M is the length of l2
*/
object Solution1 {
    def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
        val headNode = new ListNode(-1, null)
        var cur = headNode
        
        var no1 = l1;
        var no2 = l2;
        
        while(no1 != null && no2 != null) {
            if (no1.x >= no2.x){
                
                cur.next = no2
                no2 = no2.next
            }else {
                cur.next = no1
                no1 = no1.next
            }
            cur = cur.next
        }
        (no1, no2) match {
            case (_, null) => cur.next = no1
            case (null, _) => cur.next = no2
            case _ => throw new RuntimeException()
        }
        
        headNode.next
    }
}



/**
* recursive version
*/

object Solution1-2 {
    def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
        (l1, l2) match {
            case (null, _) => l2
            case (_, null) => l1
            case (a, b) => 
                if (a.x >= b.x){
                    b.next = mergeTwoLists(b.next, a)
                    b
                } else {
                    a.next = mergeTwoLists(a.next, b)
                    a   
                }
        }
    }
}