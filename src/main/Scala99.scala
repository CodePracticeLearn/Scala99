package main

import scala.annotation.tailrec

object Scala99 {

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
  }

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

}
