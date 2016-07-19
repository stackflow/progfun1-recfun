package recfun

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
    if (c < 0 || r < 0 || c > r) 0
    else if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceAux(chars: List[Char], acc: Int): Boolean = {
      chars match {
        case Nil => 0 == acc
        case '(' :: cs => balanceAux(cs, acc + 1)
        case ')' :: cs => if (acc < 1) false else balanceAux(cs, acc - 1)
        case _ :: cs => balanceAux(cs, acc)
      }
    }

    balanceAux(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (m, _) if m < 0 => 0
      case (_, cs) if cs.isEmpty => 0
      case (0, _) => 1
      case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
