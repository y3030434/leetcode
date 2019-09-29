package main.scala

/**
给出一个 32 位的有符号整数，你需要将这个整数中每位上的数字进行反转。

示例 1:

输入: 123
输出: 321
 示例 2:

输入: -123
输出: -321
示例 3:

输入: 120
输出: 21
注意:

假设我们的环境只能存储得下 32 位的有符号整数，则其数值范围为 [−231,  231 − 1]。请根据这个假设，如果反转后整数溢出那么就返回 0。

来源：力扣（LeetCode）
链接：https://leetcode-cn.com/problems/reverse-integer
著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
object ReverseInterger extends App {

  def reverse1(x: Int): Int = {

    val single = if (x < 0) -1 else 1
    var tmpInput = x
    var div = tmpInput / 10
    var mil:Int = tmpInput % 10
    var result: Int = 0
    while (div != 0) {
      if (single == 1 && result > (Integer.MAX_VALUE - mil) / 10  ) return 0
      if (single == -1 && result < (Integer.MIN_VALUE - mil) / 10 ) return 0
      result = result * 10 + mil
      tmpInput = tmpInput / 10
      div = tmpInput / 10
      mil = tmpInput % 10

    }

    if (single == 1 && result > (Integer.MAX_VALUE - mil) / 10  ) return 0
    if (single == -1 && result < (Integer.MIN_VALUE - mil) / 10  ) return 0
    result * 10 + mil


  }

  def reverse2(x:Int):Int ={
    val single = if (x < 0) -1 else 1
    var tmpInput = x
    var output = 0
    while(tmpInput != 0 ){
      val mil = tmpInput % 10
      if (single == 1 && output > (Integer.MAX_VALUE - mil) / 10  ) return 0
      if (single == -1 && output < (Integer.MIN_VALUE - mil) / 10 ) return 0
      output = output * 10 + mil
      tmpInput = tmpInput / 10
    }
    output
  }


  println(reverse1(123))
  println(reverse1(-123))
  println(reverse1(120))
  println(reverse1(1534236469))
  println(reverse1(-2147483648))

}
