package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times counts correctly") {
    new TestTrees {
      val frequencies: List[(Char, Int)] = times(string2Chars("lol it is cool"))
      assert(frequencies.contains(('l', 3)))
      assert(frequencies.contains(('o', 3)))
      assert(frequencies.contains(('i', 2)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abdabba".toList)) === "abdabba".toList)
    }
  }

  test("convert works correctly") {
    new TestTrees {
      val table = convert(t2)

      assert(table.length === 3)
      assert(table.contains(('a', List(0, 0))))
      assert(table.contains(('b', List(0, 1))))
      assert(table.contains(('d', List(1))))
    }
  }

  test("mergeCodeTables merging correctly") {
    val table1: CodeTable = List(
      ('a', List(0, 0)),
      ('b', List(0, 1)))
    val table2: CodeTable = List(
      ('c', List(1, 0)),
      ('d', List(1, 1))
    )

    val table = mergeCodeTables(table1, table2)

    assert(table.length === 4)
    assert(table.contains(('a', List(0, 0))))
    assert(table.contains(('b', List(0, 1))))
    assert(table.contains(('c', List(1, 0))))
    assert(table.contains(('d', List(1, 1))))
  }

  test("quickDecode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, quickEncode(t2)("abdabba".toList)) === "abdabba".toList)
    }
  }

  test("sortBy works correctly") {
    val list: List[Int] = List(3, 5, 2, 4, 1, 0)
    val sorted = sortBy[Int](list, (i1, i2) => i1.compareTo(i2))
    assert(sorted === List(0, 1, 2, 3, 4, 5))
    val list2: List[Int] = List(3)
    val sorted2 = sortBy[Int](list2, (i1, i2) => i1.compareTo(i2))
    assert(sorted2 === List(3))
  }

  test("border case: one char") {
    val tree = createCodeTree(List('a'))
    assert(decode(tree, quickEncode(tree)("aaa".toList)) === "aaa".toList)
  }

  test("border case: empty string") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("".toList)) === "".toList)
      assert(decode(t1, encode(t1)("".toList)) === "".toList)
      val tree = createCodeTree(List())
      assert(decode(tree, quickEncode(tree)("".toList)) === "".toList)
      assert(decode(tree, encode(tree)("".toList)) === "".toList)
    }
  }

  test("border case: char is not in tree") {
    new TestTrees {
      intercept[NoSuchElementException](quickEncode(t2)("zrz".toList))
    }
  }
}
