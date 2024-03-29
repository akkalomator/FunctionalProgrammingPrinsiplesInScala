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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
    * val s1 = singletonSet(1)
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
    val lessThanTen = { x: Int => x < 10 }
    val greaterThanMinusTen = { x: Int => x > -10 }
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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect works correctly") {
    new TestSets {
      val s = intersect(greaterThanMinusTen, lessThanTen)
      assert(!contains(s, -10), "Intersect 1")
      assert(contains(s, -9), "Intersect 2")
      assert(contains(s, 0), "Intersect 3")
      assert(contains(s, 9), "Intersect 4")
      assert(!contains(s, 10), "Intersect 5")
    }
  }

    test("diff works correctly") {
      new TestSets {
        val s = diff (lessThanTen, greaterThanMinusTen)
        assert(contains(s, -20), "Diff 1")
        assert(contains(s, -10), "Diff 2")
        assert(!contains(s, -9), "Diff 3")
        assert(!contains(s, 9), "Diff 4")
        assert(!contains(s, 10), "Diff 5")

      }
    }

    test("filter works correctly") {
      new TestSets {
        assert(!contains(filter(lessThanTen, _ > 5), 5), "filter 1")
        assert(contains(filter(lessThanTen, _ > 5), 6), "filter 2")
        assert(contains(filter(lessThanTen, _ > 5), 9), "filter 3")
        assert(!contains(filter(lessThanTen, _ > 5), 10), "filter 4")
      }
    }

  test("forall works correctly") {
    new TestSets {
      assert(forall(lessThanTen, _ < 12), "forall 1")
      assert(forall(lessThanTen, _ < 10), "forall 2")
      assert(!forall(lessThanTen, _ < 9), "forall 3")
    }
  }

  test("exists works correctly") {
    new TestSets {
      assert(exists(lessThanTen, _ < 12), "exists 1")
      assert(exists(lessThanTen, _ < 10), "exists 2")
      assert(exists(lessThanTen, _ > 8), "exists 3")
      assert(!exists(lessThanTen, _ > 9), "exists 4")
    }
  }

  test("map works correctly") {
    new TestSets {
      assert(contains(map(s1, _ * 2), 2), "map 1")
      assert(!contains(map(s1, _ * 2), 3), "map 2")
    }
  }
}
