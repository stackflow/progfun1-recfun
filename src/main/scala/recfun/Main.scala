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
    if (c == 0 || r == c) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceAux(chars: List[Char], acc: Int): Boolean = {
      (chars, acc) match {
        case (_, count) if count < 0 =>
          false
        case (string, _) if string.isEmpty =>
          acc == 0
        case _ =>
          balanceAux(chars.tail, chars.head match {
            case '(' => acc + 1
            case ')' => acc - 1
            case _ => acc
          })
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
