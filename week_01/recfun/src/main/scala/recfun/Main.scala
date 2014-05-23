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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val NO_OPEN_PARENTHESIS = 0

    def calcOpenParenthesis(openParenthesis: Int, char: Char): Int = {
      char match {
        case '(' => openParenthesis + 1
        case ')' => openParenthesis - 1
        case _ => openParenthesis
      }
    }

    def balance(chars: List[Char], openParenthesis: Int): Int = {
      val open: Int = calcOpenParenthesis(openParenthesis, chars.head)
      if (open < 0 || chars.tail.isEmpty) open
      else balance(chars.tail, open)
    }

    if (chars.isEmpty) true
    else balance(chars, NO_OPEN_PARENTHESIS) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else count(money - coins.head, coins) + count(money, coins.tail)
    }

    if (money == 0 || coins.isEmpty) 0
    else count(money, coins)
  }

}
