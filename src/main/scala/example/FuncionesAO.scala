package example

import scala.annotation.tailrec

object FuncionesAO extends App {

  // A* seq [A]
  def const[A](h:A, t:List[A]):List[A] = Const(h,t)

  def apply[A](as: A*) : List[A] ={
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  /*
  Tema Funciones de Alto Orden
   */

  /*
    Ejercicio 13. Mire que pasa cuando usted pasa Nil y Const a la función foldRight
  foldRight(List(9L,6L,7L),Nil:List[Long])(Const(_,_))
  ¿Qué piensa de la relación que existe entre foldRight y los constructores de
  datos entre listas?

   */

  def foldRight[A,B](as: List[A], z: B)(f:(A,B)=>B):B = as match {
    case Const(h,t) => f(h, foldRight(t,z)(f))
    case Nil => z
  }

  //println(foldRight(List(9L,6L,7L),Nil:List[Long])(Const(_,_)))

  /*
  Ejercicio 14. Compute la función length de una lista utilizando foldRight.
   */
  def length[A](lst:List[A]):Int = foldRight(lst,0)((_,y) => y + 1)

  //println(length(List(1,2,3,4,5)))

  /*
  Ejercicio 15. Compute la función and utilizando foldRight
   */

  def and(lst:List[Boolean]):Boolean = foldRight(lst,true)(_ && _)

  //println(and(List(true,true,true,false)))

  /*
  Ejercicio 16. La función takeWhile aplicada a un predicado p y a una lista
  lst, retorna el prefijo más largo (posiblemente vacı́o) que satisface p.
   */

  def takeWhile[A](lst: List[A])(p:A=>Boolean):List[A] = lst match {
    case Const(h,t) => if(p(h)) Const(h,takeWhile(t)(p)) else return Nil
    case Nil => Nil
  }
  //println(takeWhile(List(1,2,3,4,5))(_ < 4))

  //def takeWhileR[A](lst: List[A])(p:A=>Boolean):List[A] = foldRight(lst,Nil:List[A])((x,y) => if(p(x)) Const(x,y) else y)
  //println(takeWhileR(List(1,2,3,4,5))(_ < 4))

  /*
  Ejercicio 17. Compute la función filter utilizando foldRight
   */
/*
  def filter[A](lst:List[A])(p:A=>Boolean):List[A] = lst match {
    case Nil => Nil
    case Const(h,t) => if (p(h)) Const(h,filter(t)(p)) else filter(t)(p)
  }
  println(filter(List(1,2,1,4,3,2,4,5,6,5))(_ % 2 == 0))
*/
  def filter[A](lst:List[A])(p:A=>Boolean):List[A] = foldRight(lst,Nil:List[A])((x,y) => if(p(x)) Const(x,y) else y)

//  println(filter(List(1,2,1,4,3,2,4,5,6,5))(_ % 2 == 0))

  /*
  Ejercicio 18. Compute la funcion unzip utilizando foldRight
   */

  /*
    def foldRight[A,B](as: List[A], z: B)(f:(A,B)=>B):B = as match {
    case Const(h,t) => f(h, foldRight(t,z)(f))
    case Nil => z
  }
   */

  def unzip[A,B](lst:List[(A,B)]):(List[A],List[B]) = foldRight(lst,(Nil:List[A],Nil:List[B]))((x,y) => (Const(x._1,y._1),Const(x._2,y._2)))

  //println(unzip(List((1,"a"),(2,"b"),(3,"c"))))


  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f:(B,A)=>B):B = as match {
    case Const(h,t) => foldLeft(t,f(z,h))(f)
    case Nil => z
  }

  /*
  Ejercicio 19. Compute la función lengthL de una lista utilizando foldLeft.
   */

  def lengthL[A](lst:List[A]):Int = foldLeft(lst,0)((x,_)=> x+1)

  //println(lengthL(List(1,2,3,4,5)))

  /*
  Ejercicio 20. Compute la función andL utilizando foldLeft.
   */
  def andL(lst:List[Boolean]):Boolean = foldLeft(lst,true)(_ && _)
  //println(andL(List(true,true,true,false)))

  /*
  Ejercicio 21. La función takeWhileL utilizando foldLeft.
   */
  def takeWhileL[A](lst: List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((y,x) => if(p(x)) Const(x,y) else y)

  //println(takeWhile(List(1,2,3,4,5))(_ < 4))

  /*
  Ejercicio 22. Compute la función filter utilizando foldLeft
   */
  def filterL[A](lst:List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((y,x) => if(p(x)) Const(x,y) else y)

  println(filter(List(1,2,1,4,3,2,4,5,6,5))(_ % 2 == 0))

/*
  Ejercicio 23. Implemente la función unzipL esta lista separa una lista de tuplas
  en dos listas distintas. Compute la función unzip utilizando foldLeft.
 */

  def unzipL[A,B](lst:List[(A,B)]):(List[A],List[B]) = {
    val tup = foldLeft(lst,(Nil:List[A],Nil:List[B]))((y,x) => (Const(x._1,y._1),Const(x._2,y._2)))
    val l1 = tup._1
    val l2 = tup._2
    (foldLeft(l1,Nil:List[A])((x,y)=>Const(y,x)),foldLeft(l2,Nil:List[B])((x,y)=>Const(y,x)))
  }

  //println(unzipL(List((1,"a"),(2,"b"),(3,"c"))))


}