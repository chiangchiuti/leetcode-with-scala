
/**
* chosen solution
*
* Boyer-Moore Voting Algorithm
* time complexity N(N)
* space complexity O(1)
*/

object Solution0 {
  def majorityElement(nums: Array[Int]): Int = {
    var num = nums(0)
    var counter = 0
    nums.foreach { n =>
      if (num == n) {
        counter += 1
      } else {
        counter -= 1
        if (counter == 0) {
          num = n
          counter += 1
        }
      }
    }
    num
  }
}


/**
* Boyer-Moore Voting Algorithm
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution1 {
  def majorityElement(nums: Array[Int]): Int = {
    var num = nums(0)
    var counter = 0
    nums.foreach { n =>
      if (num == n) {
        counter += 1
      } else {
        counter -= 1
        if (counter == 0) {
          num = n
          counter += 1
        }
      }
    }
    num
  }
}

/**
* immutable during iteration
*/
object Solution1-2 {
    def majorityElement(nums: Array[Int]): Int = {
       val (ans, accumulate) = (1 until nums.length).foldLeft((nums.head, 1)) {
            case ((cur, acc), idx) =>
                val incoming = nums(idx)
                if(incoming == cur) (cur, acc + 1)
                else {
                    if(acc == 1) (incoming, 1)
                    else (cur, acc - 1)
                }
        }
        ans
    }
}

/**
* HashMap
* time complexity: O(N)
* space complexity: O(N)
*/

object Solution2 {
    def majorityElement(nums: Array[Int]): Int = {
        nums.groupBy(identity).mapValues(_.length).maxBy(_._2)._1  
    }
}


/**
* sorting array and pick middle element
* time complexity O(NlogN)
*/

object Solution3 {
    def majorityElement(nums: Array[Int]): Int = {
        nums.sorted(Ordering.Int)(nums.length / 2)
    }
}

