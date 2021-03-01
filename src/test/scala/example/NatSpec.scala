package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {
  "9.1 la funcion addNat con Cero,Suc(Cero) " should " Suc(Cero) " in {
    Nat.addNat(Cero,Suc(Cero)) shouldEqual Suc(Cero)
  }
  "9.2 la funcion addNat con Suc(Suc(Cero)),Suc(Cero) " should " Suc(Suc(Suc(Cero))) " in {
    Nat.addNat(Suc(Suc(Cero)),Suc(Cero)) shouldEqual Suc(Suc(Suc(Cero)))
  }
  "9.3 la funcion addNat con Suc(Suc(Cero)),Suc(Suc(Cero)) " should " Suc(Suc(Suc(Suc(Cero)))) " in {
    Nat.addNat(Suc(Suc(Cero)),Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Cero))))
  }
  "10.1 la funcion prodNat con Cero,Suc(Cero) " should " Cero " in {
    Nat.prodNat(Cero,Suc(Cero)) shouldEqual Cero
  }
  "10.2 la funcion prodNat con Suc(Suc(Cero)),Suc(Cero) " should " Suc(Suc(Cero)) " in {
    Nat.prodNat(Suc(Suc(Cero)),Suc(Cero)) shouldEqual Suc(Suc(Cero))
  }
  "10.3 la funcion prodNat con Suc(Suc(Suc(Cero))),Suc(Suc(Cero)) " should " Suc(Suc(Suc(Suc(Suc(Suc(Cero)))))) " in {
    Nat.prodNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
  }

}
