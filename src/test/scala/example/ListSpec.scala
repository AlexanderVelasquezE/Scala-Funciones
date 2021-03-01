package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {
  "1.1 la funcion take con n=3 y lst=(\"a\",\"b\",\"c\",\"d\",\"e\") " should " List(\"a\",\"b\",\"c\") " in {
    List.take(3,List("a","b","c","d","e")) shouldEqual List("a","b","c")
  }
  "1.2 la funcion take con n=0 y lst=(1,2,3,4) " should " Nil " in {
    List.take(0,List(1,2,3,4)) shouldEqual Nil
  }
  "1.3 la funcion take con n=6 y lst=(1.0,2.0,3.0) " should " List(1.0,2.0,3.0) " in {
    List.take(6,List(1.0,2.0,3.0)) shouldEqual List(1.0,2.0,3.0)
  }
  "2.1 la funcion init con lst=(1,2,3,4,5,6) " should " List(1,2,3,4,5) " in {
    List.init(List(1,2,3,4,5,6)) shouldEqual List(1,2,3,4,5)
  }
  "2.2 la funcion init con lst=(1) " should " Nil " in {
    List.init(List(1)) shouldEqual Nil
  }
  "3.1 la funcion split con n=3 lst=(1,2,3,4,5,6,7) " should " (List(1,2,3), List(4,5,6,7)) " in {
    List.split(3,List(1,2,3,4,5,6,7)) shouldEqual (List(1,2,3), List(4,5,6,7))
  }
  "3.2 la funcion split con n=1 lst=(1,2,3,4,5,6,7) " should " (List(1), List(2,3,4,5,6,7)) " in {
    List.split(1,List(1,2,3,4,5,6,7)) shouldEqual (List(1), List(2,3,4,5,6,7))
  }
  "3.3 la funcion split con n=8 lst=(1,2,3,4,5,6,7) " should " (List(1,2,3,4,5,6,7), Nil) " in {
    List.split(8,List(1,2,3,4,5,6,7)) shouldEqual (List(1,2,3,4,5,6,7), Nil)
  }
  "3.4 la funcion split con n=0 lst=(1,2,3,4,5,6,7) " should " (Nil, List(1,2,3,4,5,6,7)) " in {
    List.split(0,List(1,2,3,4,5,6,7)) shouldEqual (Nil, List(1,2,3,4,5,6,7))
  }
  "4.1 la funcion zip con List(1,2,3),List(true,false,true,true) " should " List((1,true),(2,false),(3,true)) " in {
    List.zip(List(1,2,3),List(true,false,true,true)) shouldEqual List((1,true),(2,false),(3,true))
  }
  "4.2 la funcion zip con List(1,2,3,4),List(false,true,false) " should " List((1,false),(2,true),(3,false)) " in {
    List.zip(List(1,2,3,4),List(false,true,false)) shouldEqual List((1,false),(2,true),(3,false))
  }
  "5 la funcion unzip con List((1,\"a\"),(2,\"b\"),(3,\"c\") " should " (List(1,2,3),List(\"a\",\"b\",\"c\")) " in {
    List.unzip(List((1,"a"),(2,"b"),(3,"c"))) shouldEqual (List(1,2,3),List("a","b","c"))
  }
  "6.1 la funcion reverse con List(\"a\",\"b\",\"c\") " should " List(\"c\", \"b\", \"a\") " in {
    List.reverse(List("a","b","c")) shouldEqual List("c", "b", "a")
  }
  "6.2 la funcion reverse con List(1,2,3,4) " should " List(4,3,2,1) " in {
    List.reverse(List(1,2,3,4)) shouldEqual List(4,3,2,1)
  }
  "6.3 la funcion reverse con Nil " should " Nil " in {
    List.reverse(Nil) shouldEqual Nil
  }
  "7.1 la funcion intersperse con 1,List(2,3,4,5) " should " List(2,1,3,1,4,1,5) " in {
    List.intersperse(1,List(2,3,4,5)) shouldEqual List(2,1,3,1,4,1,5)
  }
  "7.2 la funcion intersperse con \"a\", List(\"b\",\"c\",\"d\") " should " List(\"b\",\"a\",\"c\",\"a\", \"d\") " in {
    List.intersperse("a", List("b","c","d")) shouldEqual List("b","a","c","a", "d")
  }
  "8.1 la funcion concat con List(List(1,2,3),List(4,5,6)) " should " List(1,2,3,4,5,6) " in {
    List.concat(List(List(1,2,3),List(4,5,6))) shouldEqual List(1,2,3,4,5,6)
  }
  "8.2 la funcion concat con List(List(\"a\",\"b\"),List(\"c\",\"d\",\"e\")) " should " List(\"a\",\"b\",\"c\",\"d\",\"e\") " in {
    List.concat(List(List("a","b"),List("c","d","e"))) shouldEqual List("a","b","c","d","e")
  }
  "8.3 la funcion concat con List(List(1.0,2.0),Nil,List(3.0,4.0)) " should " List(1.0,2.0,3.0,4.0) " in {
    List.concat(List(List(1.0,2.0),Nil,List(3.0,4.0))) shouldEqual List(1.0,2.0,3.0,4.0)
  }


}
