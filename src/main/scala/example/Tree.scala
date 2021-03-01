package example

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object Tree extends App {
  /*
  Tema Arboles
   */
  /*
  Ejercicio 11. Implemente la función size que cuente el número de nodos Leaf
  y Branches en un árbol.
   */

  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }
/*
  println(size(Leaf(10)))
  println(size(Branch(Leaf(10),Leaf(20))))
  println(size(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))))

 */

  /*
  Ejercicio 12. Implemente la función depth que retorna la longitud máxima
  desde profundidad desde la raı́z a cualquier hoja.
   */
  def depth[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => { val le= depth(l); val ri=depth(r); if (le > ri) (1 + le) else (1 +ri) }
  }
/*
  println(depth(Leaf(10)))
  println(depth(Branch(Leaf(10),Leaf(20))))
  println(depth(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))))
  println(depth(Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))))
  println(depth(Branch(Branch(Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40))),Leaf(20)),Branch(Leaf(30),Leaf(40)))))
*/
}