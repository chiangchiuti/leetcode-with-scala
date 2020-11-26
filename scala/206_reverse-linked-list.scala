/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
/**
* chosen solution - iterative version
* time complexity: O(n)
* space complexity: O(1) 
*/
object Solution0 {
    def reverseList(head: ListNode): ListNode = {        
        var prev: ListNode = null
        var curr = head

        while (curr != null) {
            val hold = curr.next
            curr.next = prev
            prev = curr
            curr = hold
        }
        prev
    }
}

 /**
 * iterative version
 * time complexity: O(n)
 * space complexity: O(1)
 */
object Solution1 {
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


/** recursive version */

object Solution2 {
    def reverseList(head: ListNode): ListNode = {
        
        val curr:ListNode = null
        
        _reverseList(curr, head)
        
    }
    
    @annotation.tailrec
    def _reverseList(curr: ListNode, next: ListNode): ListNode = {
        if(next == null) {
            curr
        }else{
            val tmpNode = next.next
            next.next = curr
            _reverseList(next, tmpNode)
        }
    }
}

object Solution2-1 {
    def reverseList(head: ListNode): ListNode = {
        if(head == null) head
        else _reverseList(head)
        
    }
    
    def _reverseList(node: ListNode): ListNode = {
        if (node == null || node.next == null) {
            node
        }else {
            val newHead = _reverseList(node.next)
              // reversedHead 是返回原本的尾巴，若一開始輸入是 1 -> 2 -> 3 -> 4 -> 5  -> null , 那 reversedHead 就是 5
            // 每次 iteration 返回都是同一個 reversedHead 也就是 5
            node.next.next = node
            node.next = null
             // 每次迭代 改變的就是送進每個 function 的 listnode 的 next 與 next.next 指向
            newHead
        }
        
        
    }
}
/**
stdout:
    5 4 
    5 4 3 
    5 4 3 2 
    5 4 3 2 1 
*/

/**
The recursive version is slightly trickier and the key is to work backwards.
Assume that the rest of the list had already been reversed, now how do I reverse the front part?
Let's assume the list is: n1 → … → nk-1 → nk → nk+1 → … → nm → Ø

Assume from node nk+1 to nm had been reversed and you are at node nk.

n1 → … → nk-1 → nk → nk+1 ← … ← nm

We want nk+1’s next node to point to nk.

So,
nk.next.next = nk;

Be very careful that n1's next must point to Ø.
If you forget about this, your linked list has a cycle in it.
This bug could be caught if you test your code with a linked list of size
*/