object stack {

  //implementation of stack by me// stack has LIFO, so push and pull

  trait Stack{
    def push[T](x:T):List[T] = {
    x match {
      case Nil => Nil
      case _ => x :: Nil
    }
    def pull[T](xs:List[T], n: Int): List[T] = {

      
    }
  }

  class stacks extends Stack{

    val ls = List[Int](1,2,3,3,4,5)

    push[Int](ls)


  }

}
