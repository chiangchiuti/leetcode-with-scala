/**
* using two stack to implement
* one for push, the other for pop
*/

class MyQueue() {

  /** Initialize your data structure here. */
  private val inputStack = scala.collection.mutable.ArrayStack[Int]()
  private val outputStack = scala.collection.mutable.ArrayStack[Int]()


  /** Push element x to the back of queue. */
  def push(x: Int) {
    inputStack.push(x)

  }

  /** Removes the element from in front of queue and returns that element. */
  def pop(): Int = {
    if(outputStack.isEmpty) {
      while (inputStack.nonEmpty) {
        outputStack.push(inputStack.pop())
      }
    }
    if(outputStack.isEmpty) -1 else outputStack.pop()

  }

  /** Get the front element. */
  def peek(): Int = {
    if(outputStack.isEmpty) {
      while (inputStack.nonEmpty) {
        outputStack.push(inputStack.pop())
      }
    }
    if(outputStack.isEmpty) -1 else outputStack.head
  }

  /** Returns whether the queue is empty. */
  def empty(): Boolean = {
    outputStack.isEmpty && inputStack.isEmpty
  }

}
