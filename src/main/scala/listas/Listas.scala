package listas

object Listas extends App{


  /*
  Ejercicio 1. La función subs me permite obtener todos los subconjuntos que
  un conjunto tiene. En este ejemplo vamos a utilizar listas como un conjunto.
  Por ejemplo (En las pruebas las listas no tendrá elementos repetidos).
    scala> subs(List(1,2))
    val res0:List[List[Int]]=List(List(),List(1),List(2),List(1,2))
    scala> subs(List("a","b","c"))
    val res1:List[List[String]]=List(List(),List("a"),List("b"),List("c"),
    List("a","b"),List("a","c"),
    List("b","a"),List("b","c"),
    List("c","a"),List("c","b"),
    List("a","b","c"))
   */
/*  def subsC[A](lst:List[A]):List[List[A]] = {

  }*/
 def subs[A](lst:List[A]):List[List[A]] = {
   val a = lst match {
     case Nil => List(Nil)
     case x :: xs =>  subs(xs).map(x :: _) ::: subs(xs)
   }
   a.sortBy(_.length)
 }
  val lst = List("a","b","c","d")
  val lst2 = List(1,2)
  //println(subs(lst2))
//  println(subs(lst))
 // println(subs(lst).size)


  /*
  Ejercicio 2. La permutación es la manera de combinar todos los posibles ele-
  mentos de una lista en un conjunto de listas con todas las combinaciones posibles.
  En Scala el tipo de dato List tiene implementado un método de como obtener
  dichas listas. Nosotros vamos a implementar una función permutaciones que
  dada la siguiente firma:
  Sin utilizar el método permutations en clase alguna de Scala implementar
  la forma de computar todas las permutaciones.
   */
  def permutacionesR[A](a:A,lst:List[A]):List[List[A]] = lst match {
    case Nil => List(List(a))
    case x :: xs => (a :: (x :: xs)) :: permutacionesR(a,xs).map(x::_)
  }

  def permutaciones[A](lst:List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case x :: xs => (permutaciones(xs)).flatMap(permutacionesR(x,_))
  }

  println(permutaciones(List(1,2,3)))


}
