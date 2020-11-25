
/**
* my first commitment
* two pointer approximate
* 
* time complexity: O(N^2)
*/
object Solution1 {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    val l = nums.sorted
    // slice(0, 3) is slower 
    l.indices.foldLeft(l.take(3).sum){
      case (closestSum, idx) => twoSum(l, target, idx, closestSum)
    }

  }

  def twoSum(nums: Array[Int], target: Int, from: Int, closestSum: Int): Int = {
    val fromValue = nums(from)

    @annotation.tailrec
    def _twoSum(left: Int, right: Int, previousSum: Int): Int = {
      if(left >= right) return previousSum


      val currentSum = fromValue + nums(left) + nums(right)

      val currentDiff = math.abs(target - currentSum)
      val previousDiff = math.abs(target - previousSum)

      val newClosest = if(currentDiff > previousDiff) previousSum else currentSum


      if(currentSum < target) _twoSum(left + 1, right, newClosest)
      else if(currentSum > target) _twoSum(left, right - 1, newClosest)
      else _twoSum(left + 1, right - 1, newClosest)

    }

    _twoSum(from + 1, nums.length - 1, closestSum)
  }
}