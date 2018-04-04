object Pattermatching001 {

  def main(args: Array[String]): Unit ={

    val nonEmptySeq =Seq(1,2,3,4,5)
    val emptySeq = Seq.empty[Int]
    val nonEmptyList = List(1, 2, 3, 4, 5)
    val emptyList = Nil
    val nonEmptyVector = Vector(1, 2, 3, 4, 5)
    val emptyVector = Vector.empty[Int]
    val nonEmptyMap = Map(1->"2", 3->"4")
    val emptyMap  = Map.empty[Int,Int]



    def seqtoString[A](seq: Seq[A]): String =
      seq match {
      case Nil => "nil"
      case head+: tail => s"$head+:" + seqtoString(tail)
    }


    for (seq <- Seq( // nonEmptySeq, emptySeq, nonEmptyList, emptyList,
      nonEmptyVector, emptyVector, nonEmptyMap.toSeq, emptyMap.toSeq)) {
      println(seqtoString(seq))
    }
  }

  import math.Ordering

  case class MyList[A](list: List[A]) {
    def sortBy1[B](f: A => B)(implicit ord: Ordering[B]): List[A] =
      list.sortBy(f)(ord)
    def sort3[A](f: A => C)(implicit ord : Ordering[C]): List[A]
    def sortBy2[B : Ordering](f: A => B): List[A] = list.sortBy(f)(implicitly[Ordering[B]])
  }
  val list = MyList(List(1,3,5,2,4))
  list sortBy1 (i => -i)
  list sortBy2 (i => -i)




}
