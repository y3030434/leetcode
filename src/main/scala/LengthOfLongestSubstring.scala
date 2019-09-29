package main.scala

/**
给定一个字符串，请你找出其中不含有重复字符的 最长子串 的长度。

示例 1:

输入: "abcabcbb"
输出: 3
解释: 因为无重复字符的最长子串是 "abc"，所以其长度为 3。
示例 2:

输入: "bbbbb"
输出: 1
解释: 因为无重复字符的最长子串是 "b"，所以其长度为 1。
示例 3:

输入: "pwwkew"
输出: 3
解释: 因为无重复字符的最长子串是 "wke"，所以其长度为 3。
     请注意，你的答案必须是 子串 的长度，"pwke" 是一个子序列，不是子串。

来源：力扣（LeetCode）
链接：https://leetcode-cn.com/problems/longest-substring-without-repeating-characters
著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
object LengthOfLongestSubstring extends App {

  def lengthOfLongestSubstring(s: String): Int = {
    val strLength = s.length
    if (strLength < 2) strLength
    else {
      var leftIndex = 0
      var rightIndex = 0
      var maxNum = 0
      while (leftIndex + maxNum < strLength && rightIndex < strLength) {
        var beginIndex = leftIndex
        rightIndex += 1
        if (rightIndex == strLength && rightIndex - leftIndex > maxNum) {
          maxNum = rightIndex - leftIndex
        }
        else {
          while (beginIndex < rightIndex) {
            if (s(beginIndex) == s(rightIndex)) {
              if (rightIndex - leftIndex > maxNum) {
                maxNum = rightIndex - leftIndex
              }
              leftIndex = beginIndex + 1
            }
            beginIndex += 1
          }
        }


      }
      maxNum
    }
  }


  println(lengthOfLongestSubstring("abcabcbb"))
  println(lengthOfLongestSubstring("bbbbb"))
  println(lengthOfLongestSubstring("pwwkew"))
  println(lengthOfLongestSubstring(" "))
  println(lengthOfLongestSubstring("au"))
  println(lengthOfLongestSubstring(""))
  println(lengthOfLongestSubstring("hkcpmprxxxqw"))


  def lengthOfLongestSubstring1(s: String): Int = {
    def lengthOfShortString(s: String): Int = {
      val sLengt: Int = s.length
      if (sLengt < 2) sLengt
      else {
        var maxNum = 0
        var leftIndex = 0
        var rightIndex = 1
        var isBroker: Boolean = true
        while (rightIndex < sLengt && isBroker) {
          var beginIndex = 0
          var inSideBroker: Boolean = true
          while (beginIndex < rightIndex && inSideBroker) {
            if (s(beginIndex) == s(rightIndex)) {
              inSideBroker = false
              leftIndex = beginIndex + 1
            }
            beginIndex += 1
          }
          maxNum = beginIndex
          isBroker = inSideBroker
          rightIndex += 1
        }
        if (rightIndex == sLengt) sLengt
        else {
          val leftMaxNum = lengthOfShortString(s.substring(leftIndex))
          if (maxNum > leftMaxNum) maxNum
          else leftMaxNum
        }
      }
    }

    lengthOfShortString(s)
  }


  println(lengthOfLongestSubstring1("abcabcbb"))
  println(lengthOfLongestSubstring1("bbbbb"))
  println(lengthOfLongestSubstring1("pwwkew"))
  println(lengthOfLongestSubstring1(" "))
  println(lengthOfLongestSubstring1("au"))
  println(lengthOfLongestSubstring1(""))
  println(lengthOfLongestSubstring1("hkcpmprxxxqw"))


}
