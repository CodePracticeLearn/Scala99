abstract class monoid[A]{
  def add(x:A, y:A): A
  def unit: A


}

object Implicits001 {

implicit val stringMonoid : monoid[String] = new monoid[String] {

  def add(x: String, y: String): String = x concat y
  def unit: String = ""
}

  implicit val intMonoid: monoid[Int] = new monoid[Int]{
    def add(x: Int, y: Int): Int = x + y
    def unit: Int = 0
  }

def sum[A](xs: List[A])(implicit m: monoid[A]): A ={

  if(xs.isEmpty) m.unit
  else m.add(xs.head, sum(xs.tail))
}

  def main (args: Array[String]): Unit =
  {
    println(sum(List(1, 2, 3)))
    println(sum(List("a", "b", "c")))
  }



}
