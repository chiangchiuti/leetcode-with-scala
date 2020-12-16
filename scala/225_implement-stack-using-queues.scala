/**
 * Your MyStack object will be instantiated and called as such:
 * var obj = new MyStack()
 * obj.push(x)
 * var param_2 = obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.empty()
 */


/**
* chosen solution
* one queue version
* time complexity
*   push: O(2n+1) n is the element in queue1
*   pop: O(1)
*   top: O(1)
*/
class MyStack0() {

    /** Initialize your data structure here. */
    val queue1 = scala.collection.mutable.Queue[Int]()


    /** Push element x onto stack. */
    def push(x: Int) {
        val iter = queue1.indices
        queue1.enqueue(x)
        (iter).foreach(e => queue1.enqueue(queue1.dequeue))
        
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       if(queue1.nonEmpty) queue1.dequeue else -1
        
    }

    /** Get the top element. */
    def top(): Int = {
       queue1.headOption.getOrElse(-1)
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty
    }

}




 /**
 * my first commit
 * two queue version
 * time complexity: 
 *   push: O(1)
 *   pop: O(2n - 1)  n is the element in queue1
 *   top: O(2n - 1)
 */
class MyStack1() {

    /** Initialize your data structure here. */
    var queue1 = scala.collection.mutable.Queue[Int]()
    var queue2 = scala.collection.mutable.Queue[Int]()

    /** Push element x onto stack. */
    def push(x: Int) {
        queue1.enqueue(x)
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       while(queue1.size > 1) {
           queue2.enqueue(queue1.dequeue)
       }
    
        val ret = if(queue1.isEmpty) -1 else queue1.dequeue
        val tmp = queue1
        queue1 = queue2
        queue2 = tmp
        ret
        
    }

    /** Get the top element. */
    def top(): Int = {
        while(queue1.size > 1) {
           queue2.enqueue(queue1.dequeue)
        }
        val ret = if(queue1.isEmpty) -1 else queue1.dequeue
        val tmp = queue1
        queue1 = queue2
        queue2 = tmp
        queue1.enqueue(ret)
        ret
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty && queue2.isEmpty
    }

}

/**
* two queue version
* time complexity
*   push: O(2n+1) n is the element in queue1
*   pop: O(1)
*   top: O(1)
*/
class MyStack2() {

   import scala.collection.mutable.Queue
    /** Initialize your data structure here. */
    var queue1 = Queue.empty[Int] 


    /** Push element x onto stack. */
    def push(x: Int) {
        val queue2 = Queue(x)
        queue2.enqueueAll(queue1.dequeueAll(_ => true))
        queue1 = queue2
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
        if(queue1.isEmpty) -1 else queue1.dequeue
    }

    /** Get the top element. */
    def top(): Int = {
       queue1.headOption.getOrElse(-1)
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty
    }
}

/**
* one queue version
* time complexity
*   push: O(2n+1) n is the element in queue1
*   pop: O(1)
*   top: O(1)
*/
class MyStack3() {

    /** Initialize your data structure here. */
    val queue1 = scala.collection.mutable.Queue[Int]()


    /** Push element x onto stack. */
    def push(x: Int) {
        val iter = queue1.indices
        queue1.enqueue(x)
        (iter).foreach(e => queue1.enqueue(queue1.dequeue))
        
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       if(queue1.nonEmpty) queue1.dequeue else -1
        
    }

    /** Get the top element. */
    def top(): Int = {
       queue1.headOption.getOrElse(-1)
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty
    }

}

/**
* memo:
*   1. push entire old queue into a new queue without expanding all elements 
* time complexity:  
*     all operation are O(1) after being amortized
*   
* start  Queue()
* push1  Queue(1, Queue())
* push2  Queue(2, Queue(1, Queue()))
* push3  Queue(3, Queue(2, Queue(1, Queue())))
* push4  Queue(4, Queue(3, Queue(2, Queue(1, Queue()))))
* pop    Queue(3, Queue(2, Queue(1, Queue())))
* pop    Queue(2, Queue(1, Queue()))
*/

class MyStack4() {
  import scala.collection.mutable
  /** Initialize your data structure here. */
  var queue: mutable.Queue[Any] = mutable.Queue.empty[Any]

  /** Push element x onto stack. */
  def push(x: Int) {
    val queue2: mutable.Queue[Any] = mutable.Queue(x)
    queue2.enqueue(queue)
    queue = queue2

  }
  /** Removes the element on top of the stack and returns that element. */
  def pop(): Int = {

    if(queue.isEmpty) -1 else {
      val ret = queue.dequeue.asInstanceOf[Int]
      queue = queue.dequeue.asInstanceOf[mutable.Queue[Any]]
      ret
    }
  }
  /** Get the top element. */
  def top(): Int = {
    if(queue.isEmpty) -1 else queue.head.asInstanceOf[Int]
  }

  /** Returns whether the stack is empty. */
  def empty(): Boolean = {
    queue.size != 2
  }
}