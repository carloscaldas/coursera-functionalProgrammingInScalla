package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.Nil

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]

    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("InsertTwoElementsInEmptyHeap") = forAll { (a: Int, b: Int) =>
    val result = insert(a, insert(b, empty))
    val smallest = if (a < b) a else b
    findMin(result) == smallest
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("InsertOneElementInEmptyHeapAndDeleteMinimum") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  def toList(h: H) : List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("AnyHeapDeleteInOrderedElements") = forAll {heap: H =>
    val list = toList(heap)
    (list, list.tail).zipped.forall(_ <= _)
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other
  property("MinimumOfTwoMeldedHeap") = forAll{ (a: H, b:H)  =>
    val minA = findMin(a)
    val melded1 = meld(a,b)
    val melded2 = meld(deleteMin(a), insert(minA, b))
    toList(melded1) == toList(melded2)
  }


}


