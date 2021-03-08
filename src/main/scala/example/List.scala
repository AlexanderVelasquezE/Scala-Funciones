package example

import scala.annotation.tailrec

sealed trait List [+A]
case object Nil extends List[Nothing]
case class Const[+A](h:A,t:List[A]) extends List[A]

object List extends App {

  // A* seq [A]
  def const[A](h:A, t:List[A]):List[A] = Const(h,t)

  def apply[A](as: A*) : List[A] ={
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
  @tailrec
  def drop[A](pos:Int,lst:List[A]):List[A] = (pos,lst) match {
    case (n,Nil) => Nil
    case (0,l) => l
    case (n,Const(h,t)) => drop(n-1,t)
  }

  // A* seq [A]
  def length[A](lst:List[A]):Int= lst match {
    case Nil => 0
    case Const(h,t) => 1 + length(t)
  }
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }
  def product(ds: List[Double]):Double = ds match {
    case Nil => 1
    case Const(h,t) => h * product(t)
  }
  /*
   Tema 2 Coincidencia de patrones
*/
  /*
  Ejercicio 1 ¿Cuál es el resultado de la siguiente expresión match?
   */
  val x = List(4,5,6,7,8) match {
    case Const(x, Const(5, Const(7, _))) => x
    case Nil => 1
    case Const(x, Const(y, Const(6, Const(7, _)))) => x + y
    case Const(h, t) => h + sum(t)
    case _ => 777
  }
  //El valor que tiene x es 9

  /*
  Ejercicio 2. Implementa la función tail que remueva el primer elemento de un lista
   */
  def tail[A](lst:List[A]):List[A]= lst match {
    case Nil => Nil
    case Const(h,t) => t
  }

  /*
  Ejercicio 3. Implementa la función head que devuelva el primer elemento de la lista.
   */
  def head[A](lst:List[A]):A = lst match {
    case Const(h,t) => h
  }
  /*
    Ejercicio 4. Implemente la siguiente función. def and(lst:List[Boolean]):Boolean = ? ? ?
   */
  def and(lst:List[Boolean]):Boolean = lst match {
    case Nil => true
    case Const(h,Nil) => h
    case Const(h,t) => h && and(t)
  }
  /*
  Ejercicio 5. Implemente la siguiente función. def or(lst:List[Boolean]):Boolean = ???
   */
  def or(lst:List[Boolean]):Boolean = lst match {
    case Nil => false
    case Const(h,Nil) => h
    case Const(h,t) => h || or(t)
  }
  /*
  Ejercicio 6. Implemente la siguiente función.
   */
  def max(lst:List[Int]):Int = {
    @tailrec
    def maxr(lst:List[Int], max:Int):Int = lst match {
      case Nil => max
      case Const (h, t) => maxr (t, if (h > max) h else max)
    }
    maxr(tail(lst),head(lst))
  }
  /*
  Ejercicio 7. Implemente la siguiente función.
   */
  def min(lst:List[Int]):Int = {
    @tailrec
    def minr(lst:List[Int], min:Int):Int = lst match {
      case Nil => min
      case Const (h, t) => minr (t, if (h < min) h else min)
    }
    minr(tail(lst),head(lst))
  }
  /*
  Ejercicio 8. Implemente la siguiente función. def minMax(lst:List[Double]):(Double,Double) = ? ? ?
   */
  def minMax(lst:List[Double]):(Double,Double) = {
    def selectionmm(op:Int, n1:Double,n2:Double):Double = op match {
      case 1 => if(n1 > n2) n1 else n2
      case _ => if(n1 < n2) n1 else n2
    }
    @tailrec
    def minMaxR(lst:List[Double],mm:(Double,Double)):(Double,Double) = lst match {
      case Nil => mm
      case Const(h,t) => minMaxR(t, (selectionmm(0,h,mm._1),selectionmm(1,h,mm._2)))
    }
    minMaxR(tail(lst),(head(lst),head(lst)))
  }



  def appendOrdAsc(lst1:List[Int],lst2:List[Int]):List[Int] = (lst1, lst2) match {
    case (Nil,Nil) => Nil
    case (l1,Nil) => l1
    case (Nil,l2) => l2
    case (Const(h,t),Const(h2,t2)) => if (h<h2) Const(h,appendOrdAsc(t,Const(h2,t2))) else Const(h2,appendOrdAsc(Const(h,t),t2))
  }


  /*
  Tema 2 Construcción de listas
   */
  /*
  Ejercicio 1 Implemente la función take que se encarga de tomar dos parámetros.
  El primero un valor entero positivo n y el segundo una lista de valores de
  cualquier tiempo. Y esta función se encarga de tomar los n primeros valores, si
  existen de la lista
   */

  def take[A](n:Int,lst:List[A]):List[A] = (n,lst) match {
    case (0,_) => Nil
    case (n,Nil) => Nil
    case (n,Const(h,t)) => Const(h,take(n-1,t))
  }

  /*
  Ejercicio 2. Implemente la función init que tiene toma una lista y toma los valores
  iniciales excepto el último.
   */

  def init[A](lst:List[A]):List[A] = lst match {
    case Const(h,Nil) => Nil
    case Const(h,t) => Const(h,init(t))
  }

  /*
  Ejercicio 3. Implemente la función split, recibe dos parámetros n y una lista;
divide la primera lista en n elementos y los restantes quedan en la segunda lista.
   */

  def split[A](pos:Int,lst:List[A]):(List[A],List[A]) = {
    @tailrec
    def splitAux[A](pos:Int,lst:List[A],lst2:List[A]):(List[A],List[A]) = (pos,lst) match {
      case (a,Nil) => (lst2,Nil)
      case (0,lst) => (lst2,lst)
      case (a,Const(h,t)) => splitAux(a-1,t,addEnd(h,lst2))
    }
    splitAux(pos,lst,Nil)
  }
  //Otra version del split utilizando funciones take y drop
  def split2[A](pos:Int,lst:List[A]):(List[A],List[A]) = (take(pos,lst),drop(pos,lst))


  /*
  Ejercicio 4. Implemente la función zip esta función fusiona dos listas de tipos diferentes
  en una lista de pares del mismo tamaño. La siguiente es la firma de la función:
   */

  def zip[A,B](lst1:List[A],lst2:List[B]):List[(A,B)] = (lst1,lst2) match {
    case (Const(h,t),Nil) => Nil
    case (Nil,Const(h,t)) => Nil
    case (Const(h,t),Const(h2,t2)) => Const((h,h2),zip(t,t2))
  }

  /*
  Ejercicio 5. Implemente la función unzip esta lista separa una lista de tuplas
  en dos listas distintas.
   */

  def addEnd[A](h:A, t:List[A]):List[A] = t match {
    case Const(h1,t2) => Const(h1,addEnd(h,t2))
    case Nil => Const(h,Nil)
  }

  def unzip[A,B](lst:List[(A,B)]):(List[A],List[B]) = {
    @tailrec
    def unzipRec[A,B](lst:List[(A,B)],l1:List[A],l2:List[B]):(List[A],List[B]) = lst match {
      case Nil => (l1,l2)
      case Const(h,t) => unzipRec(t,addEnd(h._1,l1),addEnd(h._2,l2))
    }
    unzipRec(lst,Nil,Nil)
  }

  /*
  Ejercicio 6. Implemente la función reverse. Toma una lista y devuelve una
  versión invertida de la misma.
   */
  def reverse[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case Const(h,t) => addEnd(h,reverse(t))
  }
  //println(reverse(Const(3,Const(2,Const(1,Nil)))))
  /*
  Ejercicio 7. Implemente la función intersperse. Esta se encarga de entremezclar
  un valor entre los elementos originales de la lista.
   */
  def intersperse[A](elem:A,lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case Const(h,Nil) => Const(h,Nil)
    case Const(h,t) => Const(h,Const(elem,intersperse(elem,t)))
  }

  /*
  Ejercicio 8. Implemente la función concat. Es función recibe una lista de lista
  valores de un tipo A y la transforma en una lista de valores de tipo A.
   */

  def append[A](lst1:List[A],lst2:List[A]):List[A] = (lst1, lst2) match {
    case (Nil,Nil) => Nil
    case (l1,Nil) => l1
    case (Nil,l2) => l2
    case (Const(h,t),l2) => Const(h,append(t,l2))
  }

  def concat[A](lst:List[List[A]]):List[A] = lst match {
    case Nil => Nil
    case Const(h,t) => append(h,concat(t))
  }

  @tailrec
  def dropWhile[A](lst:List[A])(f:A=>Boolean):List[A] = lst match {
    case Const(h,t) if f(h) => dropWhile(t)(f)
    case _ => lst
  }

  def reduce(lst:List[Int],mod:Int)(f:(Int,Int)=>Int):Int = lst match {
    case Nil => mod
    case Const(h, t) => f(h,reduce(t,mod)(f))
  }
//  def sumR(lst:List[Int]) = reduce(lst,0)((x,y)=>x+y)
//  def mulR(lst:List[Int]) = reduce(lst,1)((x,y)=>x*y)

  def foldRight[A,B](as: List[A], z: B)(f:(A,B)=>B):B = as match {
    case Const(h,t) => f(h, foldRight(t,z)(f))
    case Nil => z
  }

  //def sumR(lst:List[Int]) = foldRight(lst,0)((x,y)=>x+y)
  def sumR(lst:List[Int]) = foldRight(lst,0)(_ + _)
  def mulR(lst:List[Int]) = foldRight(lst,1)(_ * _)

  def lengthF[A](lst:List[A]):Int = foldRight(lst,0)((_,y) => y + 1)

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f:(B,A)=>B):B = as match {
    case Const(h,t) => foldLeft(t,f(z,h))(f)
    case Nil => z
  }

  def sumL(lst:List[Int]):Int = foldLeft(lst,0)(_ + _)
  def mulL(lst:List[Int]):Int = foldLeft(lst,0)(_ * _)

  def sumarUnoR(lst: List[Int]):List[Int] = foldRight(lst,Nil:List[Int])((x,y)=>Const(x+1,y))
//  def sumarUnoL(lst: List[Int]):List[Int] = foldLeft(lst,Nil:List[Int])((x,y)=>Const(1+y,x))
  def reverseL(lst: List[Int]):List[Int] = foldLeft(lst,Nil:List[Int])((x,y)=>Const(y,x))

//  println(sumarUnoL(List(1,2,3,4,5)))

  def mapGen[A,B](lst:List[A])(f:A=>B):List[B] = lst match {
    case Nil => Nil
    case Const(h,t) => Const(f(h),mapGen(t)(f))
  }

  def map[A,B](lst:List[A])(f:A=>B):List[B] = foldRight(lst,Nil:List[B])((x,y)=> Const(f(x),y))
  def mapL[A,B](lst:List[A])(f:A=>B):List[B] = foldLeft(lst,Nil:List[B])((y,x)=> Const(f(x),y))

  //println(map(List(1,2,3,4,5))(_*2))
  //println(mapL(List(1,2,3,4,5))(_*2))

  def dropWhileR[A](list: List[A])(p: A => Boolean): List[A] = {
    def f(a: A, b: (Boolean, List[A])): (Boolean, List[A]) = b match {
      case (true, Const(h, t)) => if (p(h)) (true, t) else (false, Const(h, t))
      case (true, Nil) => (false, Nil)
      case (false, _) => b
    }
    foldRight(list, (true, list))(f)._2
  }

}