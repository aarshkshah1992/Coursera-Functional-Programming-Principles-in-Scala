package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def isParanthesis(c: Char) = c == '(' || c == ')'

    def canClose(listC: Char, stackC: Char) = stackC == '(' && listC == ')'

    @tailrec
    def checkBal(listRem: List[Char], unclosedB: List[Char]): Boolean = {
      if (listRem.isEmpty) unclosedB.isEmpty
      else if (unclosedB.isEmpty) {
        if (isParanthesis(listRem.head))
          checkBal(listRem.tail, listRem.head :: unclosedB)
        else
          checkBal(listRem.tail, unclosedB)
      }
      else {
        val topC = listRem.head
        val topUnClosed = unclosedB.head
        if (!isParanthesis(topC))
          checkBal(listRem.tail, unclosedB)
        else {
          if (canClose(topC, topUnClosed))
            checkBal(listRem.tail, unclosedB.tail)
          else
            checkBal(listRem.tail, topC :: unclosedB)
        }
      }
    }

    checkBal(chars, Nil)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }

}