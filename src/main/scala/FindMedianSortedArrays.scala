package main.scala

/**
给定两个大小为 m 和 n 的有序数组 nums1 和 nums2。

请你找出这两个有序数组的中位数，并且要求算法的时间复杂度为 O(log(m + n))。

你可以假设 nums1 和 nums2 不会同时为空。

示例 1:

nums1 = [1, 3]
nums2 = [2]

则中位数是 2.0
示例 2:

nums1 = [1, 2]
nums2 = [3, 4]

则中位数是 (2 + 3)/2 = 2.5

来源：力扣（LeetCode）
链接：https://leetcode-cn.com/problems/median-of-two-sorted-arrays
著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
object FindMedianSortedArrays extends App {

  def findMedianSortedArrays1(nums1: Array[Int], nums2: Array[Int]): Double = {
    val length = nums1.length + nums2.length
    var array1Index = 0
    var array2Index = 0
    val newArray = new Array[Int](length / 2 + 1)


    while (array1Index != nums1.length && array2Index != nums2.length && (array1Index + array2Index) <= length / 2) {
      if (nums1(array1Index) < nums2(array2Index)) {
        newArray(array1Index + array2Index) = nums1(array1Index)
        array1Index += 1
      }
      else {
        newArray(array1Index + array2Index) = nums2(array2Index)
        array2Index += 1
      }

    }

    while ((array1Index + array2Index) <= length / 2) {
      if (array1Index != nums1.length) {
        newArray(array1Index + array2Index) = nums1(array1Index)
        array1Index += 1
      }
      else {
        newArray(array1Index + array2Index) = nums2(array2Index)
        array2Index += 1
      }
    }

    if (length % 2 == 0) {
      (newArray(length / 2) + newArray(length / 2 - 1)).toDouble / 2
    }
    else {
      newArray(length / 2).toDouble
    }


  }


  def findMedianSortedArrays2(nums1: Array[Int], nums2: Array[Int]): Double = {

    val (shortArray, longArray) = {
      if (nums1.length < nums2.length) (nums1, nums2)
      else (nums2, nums1)
    }

    val shortLength = shortArray.length
    val longLength = longArray.length

    if (shortLength == 0) (longArray((longLength + 1) / 2 - 1) + longArray((longLength + 2) / 2 - 1)).toDouble / 2
    else {
      var indexLeft = 1
      var indexRight = shortLength
      val halfLength = (shortLength + longLength + 1) / 2
      var result = 0.0
      while (indexLeft <= indexRight) {
        val shortMedian = (indexLeft + indexRight) / 2
        val longMedian = halfLength - shortMedian
        if (shortMedian > indexLeft && shortArray(shortMedian - 1) > longArray(longMedian)) {
          indexRight = shortMedian - 1
        }
        else if (shortMedian < indexRight && shortArray(shortMedian) < longArray(longMedian - 1)) {
          indexLeft = shortMedian + 1
        }
        else {
          val maxLeft = {
            if(shortMedian == 0) longArray(longMedian)
            else if(longMedian == 0) shortArray(shortMedian -1)
            else if (shortArray(shortMedian - 1) > longArray(longMedian - 1)) shortArray(shortMedian - 1)
            else longArray(longMedian - 1)
          }
          if ((shortLength + longLength).&(1) != 0) {
            result = maxLeft
          }
          else {

            val minRight = {
              if (shortMedian == shortLength) longArray(longMedian)
              else if (longMedian == longLength) shortArray(shortMedian)
              else if (shortArray(shortMedian) > longArray(longMedian)) longArray(longMedian)
              else shortArray(shortMedian)
            }
            result = (maxLeft + minRight).toDouble / 2
          }
          indexLeft = indexLeft + 1


        }
      }
      result
    }
  }

  println(findMedianSortedArrays1(Array(1, 3), Array(2)))
  println(findMedianSortedArrays1(Array(1, 2), Array(3, 4)))
  println(findMedianSortedArrays1(Array(1, 2), Array()))

  println(findMedianSortedArrays2(Array(1, 3), Array(2)))
  println(findMedianSortedArrays2(Array(1, 2), Array(3, 4)))
  println(findMedianSortedArrays2(Array(1, 2), Array()))

}
