package example

import scala.annotation.tailrec

sealed trait Nat
case object Cero extends Nat
case class Suc(nat:Nat) extends Nat
case class Pre(nat:Nat) extends Nat

object Nat extends App {
  /*
  Tema 3 Definicio de tipos algebraicos los Naturales
   */
  /*
  Ejercicio 10. Implemente la función fromNatToInt que toma un número natural Nat y lo
  transforma a su valor Int.
   */
  def fromNatToInt(nat:Nat):Int = nat match {
    case Cero => 0
    case Suc(nat) => 1 + fromNatToInt(nat)
  }
  /*
  Ejercicio 11. Implemente la función fromIntToNat que tomar valores enteros
positivo (inclusive le cero) y produce el correspondiente número natural.
   */
  //Precondicion: integer >=0
  def fromIntToNat(i:Int):Nat = i match {
    case 0 => Cero
    case n => Suc(fromIntToNat(n-1))
  }
  /*
  Tema 3 Naturales
   */
  /*
  Ejercicio 9. Implemente la función addNat. Esta función recibe dos naturales y
  se encarga de sumarlos produciendo un valor correcto
   */

  def addNat(nat1:Nat,nat2:Nat):Nat = nat1 match {
    case Cero => nat2
    case Suc(nat) => addNat(nat,Suc(nat2))
  }
  /*
  println(addNat(Cero,Suc(Cero)))
  println(addNat(Suc(Suc(Cero)),Suc(Cero)))
  println(addNat(Suc(Suc(Cero)),Suc(Suc(Cero))))
*/
  /*
    Ejercicio 10. Implemente la función prodNat. Esta función realiza la multipli-
  cación de dos valores naturales.
   */

  def prodNat(nat1:Nat,nat2:Nat):Nat = {
    if (nat1 == Cero || nat2 == Cero) {
      return Cero
    }
    else if(nat1 == Suc(Cero)){
      return nat2
    }
    else if(nat2 == Suc(Cero)){
      return nat1
    }
    else{
      @tailrec
      def prodNatR(nat1:Nat,nat2:Nat, result:Nat):Nat = nat1 match {
        case Cero => result
        case Suc(n) => prodNatR(n,nat2,addNat(result,nat2))
      }
      prodNatR(nat1,nat2,Cero)
    }
  }
  /*
  println(prodNat(Cero,Suc(Cero)))
  println(prodNat(Suc(Suc(Cero)),Suc(Cero)))
  println(prodNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))))
  println(prodNat(Suc(Suc(Suc(Suc(Suc(Cero))))),Suc(Suc(Suc(Suc(Cero))))))
  */

  def minusNat(nat1:Nat,nat2:Nat):Nat = (nat1,nat2) match {
    case (n,Cero) => n
    case (Pre(n1),Suc(n2)) => minusNat(Pre(Pre(n1)),n2)
    case (Suc(n1),Suc(n2)) => minusNat(n1,n2)
    case (Cero,Suc(n2)) => minusNat(Pre(Cero),n2)
  }

  //println(minusNat(Suc(Cero),Suc(Suc(Suc(Suc(Suc(Cero)))))))

  def eq(nat1: Nat,nat2: Nat):Boolean = (nat1,nat2) match {
    case (Cero,Cero) => true
    case (Suc(n1),Cero) => false
    case (Cero,Suc(n1)) => false
    case (Suc(n1),Suc(n2)) => eq(n1,n2)
  }
  def gtNat(nat1: Nat,nat2: Nat):Boolean = (nat1,nat2) match {
    case (Suc(n1),Cero) => true
    case (Cero,Suc(n1)) => false
    case (Suc(n1),Suc(n2)) => gtNat(n1,n2)
    case (Cero,Cero) => false
  }
/*
  println(gtNat(Cero,Suc(Cero)))
  println(gtNat(Suc(Suc(Cero)),Suc(Cero)))
*/
  def lsNat(nat1: Nat,nat2: Nat):Boolean = !(gtNat(nat1,nat2))
/*
  println(lsNat(Cero,Suc(Cero)))
  println(lsNat(Suc(Suc(Cero)),Suc(Cero)))
*/
  def gteqNat(nat1: Nat,nat2: Nat):Boolean = (nat1,nat2) match {
    case (Suc(n1),Cero) => true
    case (Cero,Suc(n1)) => false
    case (Suc(n1),Suc(n2)) => gteqNat(n1,n2)
    case (Cero,Cero) => true
  }

  def lseqNat(nat1: Nat,nat2: Nat):Boolean = !(gtNat(nat1,nat2))


  def divNat(nat1:Nat,nat2:Nat):Nat = {
    if(nat2 == Cero){
      return Pre(Cero)
    }
    else if(nat2 == Suc(Cero)){
      return nat1
    }
    @tailrec
    def divNatR(n1: Nat,n2: Nat,acc: Nat):Nat = {
      if(lsNat(n1,n2)){
        return acc
      }
      else {
        divNatR(minusNat(n1,n2), n2, Suc(acc))
      }
    }
    divNatR(nat1,nat2,Cero)
  }

  println(divNat(Suc(Suc(Suc(Suc(Cero)))),Suc(Suc(Cero))))
}