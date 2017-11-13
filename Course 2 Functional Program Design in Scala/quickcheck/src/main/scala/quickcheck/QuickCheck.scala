package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertToEmpty") = forAll { (h: H, m: A) =>
    if (isEmpty(h)) isEmpty(deleteMin(insert(m, h))) else true
  }

  property("insert2elements") = forAll { (h: H, m1: A, m2: A) =>
    if (isEmpty(h)) findMin(insert(m2, insert(m1, h))) == {if (m1<=m2) m1 else m2}
    else true
  }

  property("findMinOfMeld") = forAll { (h1: H, h2: H) =>
    if ((!isEmpty(h1)) && (!isEmpty(h2))) {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      findMin(meld(h1, h2)) == {if (m1 <= m2) m1 else m2}
    } else true
  }

  property("link2heaps") = forAll { (m1: A, m2: A) =>
    val h1 = insert(m1, empty)
    val h2 = insert(m2, empty)
    findMin(deleteMin(meld(h1, h2))) == {if (m1 <= m2) m2 else m1}
  }

  property("smallestSecondTree") = forAll { (m1: A, m2: A) =>
    if (m1 <= m2) {
      val h2 = insert(m2, empty)
      val h1 = insert(m1, h2)

      findMin(deleteMin(meld(h1,h2))) == m2
    } else true
  }
}
