package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)

    val set_1_to_5 = union(union(union(union(s1, s2), s3), s4), s5)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all elements") {
    new TestSets {
      val i1 = intersect(s1, s2)
      assert(!contains(i1, 1), "Has no intersection")

      val i2 = intersect(s1, singletonSet(1))
      assert(contains(i2, 1), "Has intersection")

      val xs = union(union(s1, s2), s3)
      val ys = union(s1, s3)
      val i = intersect(xs, ys)
      assert(contains(i, 1))
      assert(contains(i, 3))
      assert(!contains(i, 2))
    }
  }

  test("difference contains all elements") {
    new TestSets {
      // diff(s, t) -> all elements of s that are not in t
      var d = diff(s1, s2)
      assert(contains(d, 1))

      var s = union(union(s1, s2), s3)
      var t = union(s1, s3)
      d = diff(s, t)
      assert(contains(d, 2))
    }
  }

  test("Filter returns the subset of a set according to a criteria") {
    new TestSets {
      val pairs = (x: Int) => x % 2 == 0
      val filtered_pairs = filter(set_1_to_5, pairs)

      assert(!contains(filtered_pairs, 1))
      assert(contains(filtered_pairs, 2))
      assert(!contains(filtered_pairs, 3))
      assert(contains(filtered_pairs, 4))
      assert(!contains(filtered_pairs, 5))
    }
  }

  test("Check if all elements satisfy the specified criteria") {
    new TestSets {
      val smaller_than_6 = (elem: Int) => elem < 6
      val greater_than_3 = (elem: Int) => elem > 3
      val smaller_than_0 = (elem: Int) => elem < 0
      val multiple_of_2 = (elem: Int) => elem % 2 == 0
      val multiple_of_1 = (elem: Int) => elem % 1 == 0

      assert(forall(set_1_to_5, smaller_than_6), "All smaller than 6")
      assert(!forall(set_1_to_5, greater_than_3), "All greater than 3")
      assert(!forall(set_1_to_5, smaller_than_0), "All smaller than 0")
      assert(forall(set_1_to_5, multiple_of_1), "All multiples of 1")
      assert(!forall(set_1_to_5, multiple_of_2), "All multiples of 2")

      val less_than_5 = (elem: Int) => elem < 5
      val ct = union(union(set_1_to_5, singletonSet(7)), singletonSet(1000))
      assert(!forall(ct, less_than_5), "filter of {1,3,4,5,7,1000} for _ < 5")
    }
  }

  test("Check if at least one element satisfy the specified criteria") {
    new TestSets {
      val multiple_of_2 = (elem: Int) => elem % 2 == 0
      val multiple_of_7 = (elem: Int) => elem % 7 == 0

      assert(exists(set_1_to_5,  multiple_of_2), "At least one multiple of 2")
      assert(!exists(set_1_to_5, multiple_of_7), "At least one multiple of 7")
    }
  }

  test("Maps the elements of a set to other values according to the specified criteria") {
    new TestSets {
      val set_1_to_3 = union(union(s1, s2), s3)
      val multiply_by_3 = (elem: Int) => elem * 3

      val multiples_of_3 = map(set_1_to_3, multiply_by_3)

      assert(contains(multiples_of_3, 3), "1 * 3 = 3")
      assert(contains(multiples_of_3, 6), "2 * 3 = 6")
      assert(contains(multiples_of_3, 9), "3 * 3 = 9")

      assert(!contains(multiples_of_3, 1), "Doesn't contain 1")
      assert(!contains(multiples_of_3, 2), "Doesn't contain 2")
      assert(!contains(multiples_of_3, 4), "Doesn't contain 4")
    }
  }




}
