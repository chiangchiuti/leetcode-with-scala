
/**
*  using map
* time complexity: O(N)
* space complexity: O(N)
*/

object Solution {
    def majorityElement(nums: Array[Int]): Int = {
        nums.groupBy(identity).mapValues(_.length).maxBy(_._2)._1  
    }
}


/**
* sort array and pick middle element
* time complexity O(NlogN)
*/

object Solution {
    def majorityElement(nums: Array[Int]): Int = {
        nums.sorted(Ordering.Int)(nums.length / 2)

    }
}

/**
* Boyer-Moore Voting Algorithm
* time complexity N(N)
* space complexity O(1)
*/

object Solution {
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