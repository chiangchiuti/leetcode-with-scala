
/**
* selected Solution
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
* scala foldLeft version
*/

object Solution0-1 {
    def majorityElement(nums: Array[Int]): Int = {
      
        nums.takeRight(nums.length - 1).foldLeft(nums(0), 1) {
            case ((currtMaj, counter), n) if currtMaj == n => (currtMaj, counter + 1)
            case ((currtMaj, counter), n) if currtMaj != n && counter > 1 =>  (currtMaj, counter - 1)
            case ((currtMaj, counter), n)  => (n, 1)

        }._1
    }
}
 




/**
*  using map
* time complexity: O(N)
* space complexity: O(N)
*/

object Solution1 {
    def majorityElement(nums: Array[Int]): Int = {
        nums.groupBy(identity).mapValues(_.length).maxBy(_._2)._1  
    }
}


/**
* sort array and pick middle element
* time complexity O(NlogN)
*/

object Solution2 {
    def majorityElement(nums: Array[Int]): Int = {
        nums.sorted(Ordering.Int)(nums.length / 2)

    }
}

