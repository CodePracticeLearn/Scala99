package main

import scala.annotation.tailrec

object Scala99Problems {

  def main(args: Array[String]) = {

    //p01 last(List(1, 1, 2, 3, 5, 8))

    def last[T](ls: List[T]): T = {
      ls.last
    }

    println("the last element is:" + last[String](List("a", "b")))

    //p02 penultimate(List(1, 1, 2, 3, 5, 8))

    def penultimate[T](xs: List[T]): T = xs match {
      case x :: y :: Nil  => x
      case x :: y :: rest => penultimate(y :: rest)
      case _              => throw new Exception("error while reading List")
    }

    println("last but one element" + penultimate(List("a", "b")))

    //p03 find the KTh element nth(2, List(1, 1, 2, 3, 5, 8))

    @tailrec
    def nth[T](n: Int, list: List[T]): Option[T] = (n, list) match {
      case (0, head :: tail) => Some(head)
      case (_, Nil)          => None
      case (x, _) if x < 0   => None
      case (x, head :: tail) => nth(x - 1, tail)
    }

    println("kth element" + nth(2, List(1, 2, 3, 8)))

    //p04 length(List(1, 1, 2, 3, 5, 8))

    def length[T](ls: List[T]): Int = {
      ls match {
        case Nil          => 0
        case head :: tail => 1 + length(tail)
      }
    }

    println("the length of list is" + length(List(1, 2, 3, 4)))

    //p05 for reversing a list reverse(Lit(1,2,3,4))

    def reverse[T](ls: List[T]): List[T] = {
      ls match {
        case Nil      => Nil
        case x :: Nil => List(x)
        case x :: y   => reverse(y) ::: List(x)
      }
    }

    println("the reverse of list is" + reverse(List(6, 4, 8, 3)))

    //p06 find if palindrome

    def isPalindrome[T](list: List[T]): Boolean = list == reverse(list)

    println("Iam a palindrome" + isPalindrome(List(1, 2, 1)))

    //p07 flatten(List(List(1, 1), 2, List(3, List(5, 8))))

    def flatten(ls: Any): List[Any] = { //can also be done with ls:List[Any]
      ls match {
        case Nil        => Nil
        case head :: xs => flatten(head) ::: flatten(xs)
        case q          => List(q)
      }
    }

    println("flattened" + flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

    //p08 compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

    def compress[T](ls: List[T]): List[T] = {
      ls match {
        case Nil         => Nil
        case head :: Nil => ls
        case first :: second :: xs =>
          if (first == second)
            compress(second :: xs)
          else
            first :: compress(second :: xs)
      }
    }

    println(
      "compressed" + compress(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

    //p09 pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    //res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

    def pack[T](list: List[T]): List[List[T]] = {

      def packInner(acc: List[T], l: List[T]): List[List[T]] = l match {
        case head :: tail if acc.isEmpty      => packInner(List(head), tail)
        case head :: tail if head == acc.head => packInner(head :: acc, tail)
        case head :: tail if head != acc.head =>
          acc :: packInner(List(head), tail)
        case Nil => List(acc)
      }

      packInner(List(), list)
    }

    println("packed" + pack(List(1, 1, 1, 2, 3, 1, 1)))

    //encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    //res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

    def encode[T](ls: List[T]): List[(Int, Option[T])] = {
      ls match {
        case Nil => List()
        case x =>
          pack(x).map { elemList =>
            (elemList.size, elemList.headOption)
          } // remove headOption here and get the ususal result
      }

    }

    println(
      "encoded" + encode(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

    //p11 encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    //can be done with encode method in very short way
    def encodeModified[T](ls: List[T]) = {
      ls match {
        case Nil => List()
        case x =>
          pack(x).map { elemList =>
            elemList.size match {
              case 1  => elemList.headOption
              case sz => (sz, elemList.headOption)
            } // remove headOption here and get the usual result
          }

      }

    }

    //p12 decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    //res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    def decode[T](ls: List[(Int, T)]) = ls.flatMap { a =>
      a match {
        case (int, item) =>
          (1 to int).map(_ => item) //any value of range print the item
      }
    }

    println(
      "decode" + decode(
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  }

  //p13 encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  //solution is similar to P09

  //p14 duplicate(List('a, 'b, 'c, 'c, 'd))
  //res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def duplicate[T](ls: List[T]) = {
    ls match {
      case Nil => List()
      case xs  => xs.map(a => (a, a))

    }
  }

  println("duplicate list" + duplicate(List(1, 2, 3)))

  //p15  duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  //res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

  def duplicateN[T](n: Int, ls: List[T]) = {
    ls.flatMap(a => (1 to n).map(_ => a))

  }

  println("duplicate listN" + duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

  //p16 drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  def drop[T](n: Int, ls: List[T]) = {
    //failure cases like list blank etc are not written
    ls.zipWithIndex filter { e =>
      (e._2 + 1) % n != 0
    } map {
      _._1
    }
  }

  println("dropped" + drop(5, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  //hereafter problems are solved with existing list methods , to get to know these methods, check the
  //source code

  //p17 split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

  def splitN[T](n: Int, ls: List[T]) = {
    //failure cases like list blank etc are not written
    (ls.slice(0, n), ls.slice(n + 1, ls.length))
  }

  println(
    "split of list" + splitN(4,
                             List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  //p18 scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: List[Symbol] = List('d, 'e, 'f, 'g)

  println("sliced" + List(1, 2, 3, 4, 5, 6).slice(3, 7))

  //p19 rotate(2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

  def rotate[T](n: Int, ls: List[T]) = {
    ls match {
      case Nil       => List()
      case xs :: Nil => xs
      case xs :: tail =>
        if (n > 0)
          ls.slice(n + 1, ls.length) ::: ls.slice(0, n)
        else
          ls.slice((ls.length + n), ls.length) ::: ls.slice(0, ls.length + n)

    }
  }

  println(
    "rotated" + rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  //p20 removeAt(1, List('a, 'b, 'c, 'd))
  //res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

  def removeAt[T](n: Int, ls: List[T]) = {

    (ls.take(n) ::: ls.drop(n).tail, ls(n))
  }

  println("removedAt" + removeAt(1, List('a, 'b, 'c, 'd)))

}
