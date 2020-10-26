
/**
* select solution
* build-in linkedHashMap
* time complexity: O(1)
*/
class LRUCache0(_capacity: Int) {

  private val capacity = _capacity
  val cache = collection.mutable.LinkedHashMap[Int, Int]()

  def get(key: Int): Int = {
    cache.get(key).map{
      value =>
        cache.remove(key)
        cache.update(key, value)
        value
    }.getOrElse(-1)
  }

  def put(key: Int, value: Int): Unit = {
    cache.get(key) match {
      case Some(_) =>
        cache.remove(key)
        cache.update(key, value)

      case None =>
        if(cache.size >= capacity){
          cache.remove(cache.head._1)
        }
        cache.put(key, value)
    }
  }
}
.

/**
* my first commitment
* implement with linked list
* time complexity:
*    get, put, delete: O(N)
*/
case class Node(key: Int, var value: Int, var next: Node = null)

class LRUCache1(_capacity: Int) {
  private val head = Node(Int.MinValue, -1, null)
  private val capacity = _capacity


  def get(key: Int): Int = {
    var preNode = head
    var current = head.next


    // find the key in linkedList
    while(current != null && current.key != key) {
      preNode = current
      current = current.next
    }


    if(current != null) {
      deleteNode(preNode)
      prepend(current)
      current.value
    } else {
      -1
    }

  }

def put(key: Int, value: Int) {
    var prepreNode = head
    var preNode = head
    var current = preNode.next
    var count = 0

    while(current != null && current.key != key) {
      prepreNode = preNode
      preNode = current
      current = current.next
      count += 1
    }
    if(current != null) {
      current.value = value
      deleteNode(preNode)
      prepend(current)

    }else {

      if(count >= this.capacity) {
        // delete node
        prepreNode.next = null
//        deleteTail()
      }
      prepend(Node(key, value, null))
    }

  }

  private def deleteNode(preNode: Node) {
    val deleteOne = preNode.next
    if(deleteOne != null) {
      preNode.next = deleteOne.next
    }
  }
  
  private def deleteTail(): Unit ={
    var preNode = head
    var current = preNode.next
    while(current != null && current.next != null) {
      preNode = current
      current = current.next
    }
    deleteNode(preNode)
  }

  private def prepend(newNode: Node) {
    newNode.next = head.next
    head.next = newNode
  }

  private def traversal(): Unit = {
    var node = head.next

    while(node != null) {
      print(s"(key: ${node.key} value: ${node.value})")
      node = node.next
    }
    println(" ")
  }
  
}

/**
* double linked list + hashset
*time complexity:
*    get, put, delete: O(1)
*/

case class Node(key: Option[Int], var value: Int, var pre:Node = null, var next: Node = null)
class LRUCache2(_capacity: Int) {

  private val capacity = _capacity
  private var currentSize = 0
  private val head = Node(None, -1)
  private var tail = Node(None, -1)
  head.next = tail
  tail.pre = head

  private val cache = collection.mutable.HashMap[Int, Node]()

  def get(key: Int): Int = {
    cache.get(key).map{ node =>
      removeNode(node)
      prependNode(node)
      node.value
    }.getOrElse(-1)
  }

  def put(key: Int, value: Int) {
    val node = cache.get(key) match {
      case Some(n) =>
        n.value = value
        removeNode(n)
        prependNode(n)
        n
      case None =>
        if(currentSize >= capacity) {
          cache.remove(tail.pre.key.get)
          removeTail()
          currentSize -= 1
        }
        currentSize += 1
        prependNode(Node(Some(key), value))
        head.next
    }
    cache += (key -> node)
  }
  private def removeTail(): Unit ={
    val lastNode = tail.pre
    removeNode(lastNode)
  }

  private def prependNode(node: Node): Unit = {
    node.next = head.next
    node.pre = head

    head.next.pre = node
    head.next = node
  }
  private def removeNode(node: Node): Unit = {
    node.pre.next = node.next
    node.next.pre = node.pre
  }
}


/**
* build in linkedHashMap
*/
class LRUCache3(_capacity: Int) {

  private val capacity = _capacity
  val cache = collection.mutable.LinkedHashMap[Int, Int]()

  def get(key: Int): Int = {
    cache.get(key).map{
      value =>
        cache.remove(key)
        cache.update(key, value)
        value
    }.getOrElse(-1)
  }

  def put(key: Int, value: Int): Unit = {
    cache.get(key) match {
      case Some(_) =>
        cache.remove(key)
        cache.update(key, value)

      case None =>
        if(cache.size >= capacity){
          cache.remove(cache.head._1)
        }
        cache.put(key, value)
    }
  }
}
.
