
/**
* my first commit
* two pointer
* time complexity: O(N^2)
*/
object Solution1 {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    val l = nums.sorted

    l.indices.foldLeft(l.take(3).sum) {  // slice(0, 3) is slower 
      case(min, idx) =>
        twoSum(l, target, min, idx)
    }
  }
  def twoSum(nums: Array[Int], target: Int, min: Int, from: Int): Int = {
    val fromValue = nums(from)
    
    @annotation.tailrec
    def loop(i: Int, j: Int, min: Int): Int = {

      if(i < j ){
        val sum = fromValue + nums(i) + nums(j)
        val newMin = if(math.abs(target - sum) <  math.abs(target - min)) sum else min
        if(sum > target) loop(i, j - 1, newMin)
        else if (sum < target) loop(i + 1, j, newMin)
        else  loop(i + 1, j - 1, newMin)
      }else {
        min
      }
    }
    loop(from + 1, nums.length - 1, min)
  }
}