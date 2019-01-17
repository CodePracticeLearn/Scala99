package main

object Scala99 {


  def main (args:Array[String]) = {

 //p01 last(List(1, 1, 2, 3, 5, 8))

    def last[T](ls:List[T]):T = {
      ls.last
    }

    println("the last element is:" +last[String](List("a","b")))

    //p02 penultimate(List(1, 1, 2, 3, 5, 8))


  }


}
