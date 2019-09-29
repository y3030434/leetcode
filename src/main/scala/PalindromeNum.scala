package main.scala

/**
判断一个整数是否是回文数。回文数是指正序（从左向右）和倒序（从右向左）读都是一样的整数。

示例 1:

输入: 121
输出: true
示例 2:

输入: -121
输出: false
解释: 从左向右读, 为 -121 。 从右向左读, 为 121- 。因此它不是一个回文数。
示例 3:

输入: 10
输出: false
解释: 从右向左读, 为 01 。因此它不是一个回文数。

来源：力扣（LeetCode）
链接：https://leetcode-cn.com/problems/palindrome-number
著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
object PalindromeNum extends App {

  /*
  280ms
   */
  def isPalindrome1(x: Int): Boolean = {
    if (x < 0) return false
    if (x == 0) true
    else {
      val eleArray = new Array[Int](32)
      var index = -1
      var input = x
      while (input > 0) {
        index += 1
        eleArray(index) = input % 10
        input = input / 10
      }
      var beginIndex = 0
      while (beginIndex < index) {
        if (eleArray(beginIndex) != eleArray(index)) return false
        beginIndex += 1
        index -= 1
      }
      true

    }
  }

  def isPalindrome2(x: Int): Boolean = {
    if (x < 0) return false
    if (x < 10) true
    else {
      var leftDiv:Int = 10
      var rightDiv:Int = leftDiv
      var leftTmpInput = x
      var rightTmpInput = x
      while (x / rightDiv  >= 10) {
        rightDiv = rightDiv * 10
      }
      while(leftDiv <= rightDiv){
        if (leftTmpInput % 10 != rightTmpInput / rightDiv) return false

        leftTmpInput = leftTmpInput / 10
        rightTmpInput = rightTmpInput % rightDiv
        leftDiv = leftDiv * 10
        rightDiv = rightDiv / 10
      }

      true

    }

  }


  println(isPalindrome1(-123))
  println(isPalindrome1(0))
  println(isPalindrome1(10))
  println(isPalindrome1(121))
  println(isPalindrome2(1))
  println(isPalindrome1(9999))
  println(isPalindrome1(1410110141))

  println("I am a line")
  println(isPalindrome2(-123))
  println(isPalindrome2(0))
  println(isPalindrome2(10))
  println(isPalindrome2(121))
  println(isPalindrome2(1))
  println(isPalindrome2(9999))
  println(isPalindrome2(1410110141))

}
