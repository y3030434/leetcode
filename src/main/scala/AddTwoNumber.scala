package main.scala

/**
给出两个 非空 的链表用来表示两个非负的整数。其中，它们各自的位数是按照 逆序 的方式存储的，并且它们的每个节点只能存储 一位 数字。

如果，我们将这两个数相加起来，则会返回一个新的链表来表示它们的和。

您可以假设除了数字 0 之外，这两个数都不会以 0 开头。

示例：

输入：(2 -> 4 -> 3) + (5 -> 6 -> 4)
输出：7 -> 0 -> 8
原因：342 + 465 = 807

  */
object AddTwoNumber extends App {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  def addTwoNumbers1(l1: ListNode, l2: ListNode): ListNode = {

    def addNumber(x: Int, y: Int, carry: Int): (ListNode, Int) = {
      val addResult = x + y + carry
      (new ListNode(addResult % 10), addResult / 10)
    }

    var carry: Int = 0
    var leftListNode = l1.next
    var rightListNode = l2.next
    var resultListNode: ListNode = null
    val result = addNumber(l1.x, l2.x, carry)
    carry = result._2
    resultListNode = result._1
    var tmpListNode = resultListNode
    while (leftListNode != null && rightListNode != null) {
      val result = addNumber(leftListNode.x, rightListNode.x, carry)
      carry = result._2
      tmpListNode.next = result._1
      leftListNode = leftListNode.next
      rightListNode = rightListNode.next
      tmpListNode = tmpListNode.next
    }
    val sampleNodeList = new ListNode(0)
    while (leftListNode != null) {
      val result = addNumber(leftListNode.x, sampleNodeList.x, carry)
      carry = result._2
      tmpListNode.next = result._1

      leftListNode = leftListNode.next
      tmpListNode = tmpListNode.next
    }

    while (rightListNode != null) {
      val result = addNumber(rightListNode.x, sampleNodeList.x, carry)
      carry = result._2
      tmpListNode.next = result._1

      rightListNode = rightListNode.next
      tmpListNode = tmpListNode.next
    }

    if (carry != 0) tmpListNode.next = new ListNode(carry)

    resultListNode
  }


  def addTwoNumbers2(l1: ListNode, l2: ListNode): ListNode = {

    val samleListNode = new ListNode(0)

    def addNum(left: ListNode, right: ListNode, carry: Int): ListNode = {
      val sum = left.x + right.x + carry
      val restListNode = new ListNode(sum % 10)
      if (left.next == null && right.next == null) {
        if (sum / 10 > 0) {
          restListNode.next = new ListNode(1)
        }
      }
      else {
        if (left.next == null) {
          restListNode.next = addNum(samleListNode, right.next, sum / 10)
        }
        else if (right.next == null) {
          restListNode.next = addNum(left.next, samleListNode, sum / 10)
        }
        else {
          restListNode.next = addNum(left.next, right.next, sum / 10)
        }
      }

      restListNode
    }

    addNum(l1, l2, 0)

  }

  val leftSampleNode = new ListNode(2)
  leftSampleNode.next = new ListNode(4)
  leftSampleNode.next.next = new ListNode(3)

  val rightSampleNode = new ListNode(5)
  rightSampleNode.next = new ListNode(6)
  rightSampleNode.next.next = new ListNode(4)

  var result = addTwoNumbers1(leftSampleNode, rightSampleNode)
  while (result != null) {
    println(result.x)
    result = result.next
  }


  result = addTwoNumbers2(leftSampleNode, rightSampleNode)
  while (result != null) {
    println(result.x)
    result = result.next
  }

}
