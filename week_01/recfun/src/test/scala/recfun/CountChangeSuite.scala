package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  test("countChange: return zero when no coins") {
    assert(countChange(10, List()) === 0)
  }

  test("countChange: return zero when amount is zero") {
    assert(countChange(0, List(1, 2, 3)) === 0)
  }

  test("countChange: return exact number for single coins") {
    assert(countChange(4, List(1)) === 1)
  }

  test("countChange: return exact number using a single coin") {
    assert(countChange(2, List(1, 2)) === 2)
  }

  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
}
