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
      assert(!contains(s1, 2), "Singleton-2")
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
  test("intersection contains all elements of each set") {
    new TestSets {
      val sUnion12 = union(s1, s2)
      val sUnion23 = union(s2, s3)
      val sInter = intersect(sUnion12,sUnion23)
      assert(contains(sInter, 2), "intersection 2")
      assert(!contains(sInter, 1), "intersection 1")
      assert(!contains(sInter, 3), "intersection 3")
    }
  }
  test("diff") {
    new TestSets {
      val sUnion12 = union(s1, s2)
      val sDiff = diff(sUnion12,s1) // {2}
      assert(contains(sDiff, 2), "diff 2")
      assert(!contains(sDiff, 1), "diff 1")
      assert(!contains(sDiff, 3), "diff 3")
    }
  }
  test("filter") {
    new TestSets {
      val sUnion12 = union(s1, s2)
      def p ={(x:Int) => x<2}
      val sUnion12_filer = filter(sUnion12,p)
      assert(contains(sUnion12_filer, 1), "filter 1")
      assert(!contains(sUnion12_filer, 2), "filter 2")
      assert(!contains(sUnion12_filer, 3), "filter 3")
    }
  }
  test("forall") {
    new TestSets {
      val sUnion12 = union(s1, s2)
      def p_T ={(x:Int) => x<3}
      def p_F ={(x:Int) => x<2}
      assert(forall(sUnion12,p_T),"forall true")
      assert(!forall(sUnion12,p_F),"forall false")
    }
  }
  test("exist") {
    new TestSets {
      val sUnion12 = union(s1, s2)
      def p_T ={(x:Int) => x<2}
      def p_F ={(x:Int) => x<1}
      assert(exists(sUnion12,p_T),"exist true")
      assert(!exists(sUnion12,p_F),"exist false")
    }
  }
  test("map") {
    new TestSets {
      val sUnion12 = union(s1, s2)
      val sMap = map(sUnion12,{(x:Int) => x*x})
      assert(contains(sMap,1), "map 1 => 1")
      assert(contains(sMap,4), "map 2 => 4")
      assert(!contains(sMap,2), "!map 2")
    }
  }
}
