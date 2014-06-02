package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'a')) is equal to List(('a', 2), ('b', 1))") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("times(List('a', 'b', 'a', 'c', 'b', 'b')) is equal to List(('a', 2), ('b', 3), ('c', 1))") {
    assert(times(List('a', 'b', 'a', 'c', 'b', 'b')) === List(('a', 2), ('b', 3), ('c', 1)))
  }

  test("makeLeafList(List(('a', 2)), ('b', 4))") {
    assert(makeLeafList(List(('a', 2), ('b', 4))) === List(Leaf('a', 2), Leaf('b', 4)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton(List[CodeTree])]") {
    new TestTrees {
      assert(singleton(List(t1  )) === true)
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("combine elements of an empty list") {
    val emptyList: List[CodeTree] = List()
    assert(combine(emptyList) === emptyList)
  }

  test("combine elements of a list with single element") {
    val singleton: List[CodeTree] = List(Leaf('a', 1))
    assert(combine(singleton) === singleton)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until with two elements") {
    val l1 = Leaf('a', 1)
    val l2 = Leaf('b', 2)
    val l3 = Leaf('c', 3)

    val l1l2 = Fork(l1, l2, List('a', 'b'), 3)
    val l1l2_l3 = Fork(l1l2, l3, List('a', 'b', 'c'), 6)
    assert(until(singleton, combine)(List(l1, l2, l3)) === List(l1l2_l3))
  }

  test("createCodeTree") {
    val a = Leaf('a', 3)
    val b = Leaf('b', 2)
    val c = Leaf('c', 1)
    val cb = Fork(c, b, List('c', 'b'), 3)
    val cb_a = Fork(cb, a, List('c', 'b', 'a'), 6)
    assert(createCodeTree(List('a', 'b', 'a', 'c', 'b', 'a')) === cb_a)
  }

  test("decode t1") {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    assert(decode(t1, List(0, 1)) === List('a', 'b'))
    assert(decode(t1, List(1, 0)) === List('b', 'a'))
    println(decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
