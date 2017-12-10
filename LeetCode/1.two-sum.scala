object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
        var low = 0
        var high = nums.length - 1
        val ns = nums.zip(Array.range(0, nums.length)).sortBy(_._1)
        while (ns(low)._1 + ns(high)._1 != target) {
            if (ns(low)._1 + ns(high)._1 < target) {
                low += 1
            } else {
                high -= 1
            }
        }
        Array(ns(low)._2, ns(high)._2)
    }

    def test = twoSum(Array(2, 7, 11, 15), 9)
}
