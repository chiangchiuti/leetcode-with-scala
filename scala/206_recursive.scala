/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
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


object Solution2 {

    def reverseList(head: ListNode): ListNode = {
        if(head == null || head.next == null) head
        else{
            val reversedHead = reverseList(head.next)
            // reversedHead 是返回原本的尾巴，若一開始輸入是 1 -> 2 -> 3 -> 4 -> 5  -> null , 那 reversedHead 就是 5
            // 每次 iteration 返回都是同一個 reversedHead 也就是 5

            head.next.next = head
            head.next = null
            // 每次迭代 改變的就是送進每個 function 的 listnode 的 next 與 next.next 指向
            printNode(reversedHead)
            reversedHead
        }
    }

    def printNode(node: ListNode) {
        var n = node
        while(n != null) {
            print(s"${n.x} ")
            n = n.next
        }
        println("")
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