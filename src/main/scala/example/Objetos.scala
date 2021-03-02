package example

object Objetos extends App {
  //println(comp.cubo(5.0F))

  //println(comp2.cubo(5L))

  val gato1 = new Gato("IO","Fawn","Churrus")
  val gato2 = new Gato("Make","Red","Leche")
  val gato3 = new Gato("Docker","Blue","Cuido")
  //println(gato1.nombre,gato1.color,gato1.comida)
  //println(gato2.nombre,gato2.color,gato2.comida)
  //println(gato3.nombre,gato3.color,gato3.comida)

  //println(VentaDeChurrus.despachar(gato1))
  //println(VentaDeChurrus.despachar(gato2))

  val conductor1 = new Conductor("Aĺex","Velasquez",10,10)
  val conductor2 = new Conductor("Daniel","Torres",16,6)
  val conductor3 = new Conductor("Carlos","Toro",10,1)
  val escuderia1 = new Escuderia("F1",conductor1)
  val escuderia2 = new Escuderia("F2",conductor2)
  val escuderia3 = new Escuderia("F1",conductor3)
//  println(conductor1.getCarrerasNoTerminadas(),conductor2.getCarrerasNoTerminadas(),conductor3.getCarrerasNoTerminadas())
//  println(escuderia1.getConductor())

  val contador1 = new Contador(10).incr().decr().incr().incr().cont
  val contador2 = new Contador(10)
  //println(contador1)
  //println(contador2.cont)

  val contador81 = new Contador8(10).incr(7).decr().incr(4).cont
  val contador82 = new Contador8(10).incr().decr(15).incr().cont
  //println(contador81)
  //println(contador82)

  val sumador1 = new Sumador(7)
  val sumador2 = new Sumador(12)
  val contador91 = new Contador9(10).ajuste(sumador1).cont
  val contador92 = new Contador9(10).ajuste(sumador2).cont
  //println(contador91)
  //println(contador92)

}
/*
  Ejercicio 1. Defina un objeto llamado comp con un método llamado cuadrado
  que acepte un valor de tipo flotante (Float) y otro método cubo que acepte un
  valor de tipo doble y utilizando el método cuadrado compute el cubo del valor
  entrado.
 */
object comp {
  def cuadrado(valor:Float):Float = {
    valor * valor
  }
  def cubo(valor:Float):Float = {
    valor * cuadrado(valor)
  }

}
/*
  Ejercicio 2. Pegue y copie el anterior objeto y lo renombra comp2, pero se debe
  cambiar todos los tipos para que utilicen valores de tipo Long.
 */

object comp2 {
  def cuadrado(valor:Long):Long = {
    valor * valor
  }
  def cubo(valor:Long):Long = {
    valor * cuadrado(valor)
  }

}

/*
Ejercicio 3. Escriba el siguiente código y utilizando el REPL pruebe la salida
del siguiente programa, mostrando el orden de ejecución de las instrucciones.

scala> object prueba {
     | def x = {
     | println("x")
     | 1
     | }
     | val y = {
     | println("y")
     | x+2
     | }
     | def z = {
     | println("z")
     | x
     | x + "c"
     | }
     | }
defined object prueba

Una vez cargado el objeto en el REPL ejecute la siguiente instrucción.

scala> prueba.x + prueba.y + prueba.z
y
x
x
z
x
x
res2: String = 41c

 */

/*
Ejercicio 4. Dada la siguiente tabla definir la clase Gato y crear un objeto de
cada gato de la siguiente tabla.
Nombre    Color   Comida
  IO      Fawn    Churrus
 Make     Red     Leche
Docker    Blue    Cuido
 */
class Gato(val nombre:String,val color:String,val comida:String) {

}

/*
  Ejercicio 5. Defina un objeto VentaDeChurrus con un método despachar. Este
  método debe aceptar un gato y retornar true (Verdadero) si la comida favorita del
  gato son los Churrus y falso de otra forma
 */
object VentaDeChurrus {
  def despachar(g:Gato):Boolean = g.comida == "Churrus"
}

/*
  Ejercicio 6. Implementar las siguiente clases que se observan en la figura
 */

class Conductor(nombre:String, apellido:String, totalCarreras:Int, carrerasTerminadas:Int){
  def getNombre():String = nombre
  def getApellido():String = apellido
  def getTotalCarreras():Int = totalCarreras
  def getCarrerasTerminadas():Int = carrerasTerminadas
  def getCarrerasNoTerminadas():Int = totalCarreras - carrerasTerminadas
}

class Escuderia(nombre:String,conductor: Conductor) {
  def getNombre():String = nombre
  def getConductor():Conductor = conductor
}

/*
  Ejercicio 7. Implementar la clase Contador que se observa en la figura 2. El
  constructor de la clase debe tomar un entero. Los métodos incr y decr deben
  retonar ambos un nuevo contador Counter.
  Aquı́ un ejemplo de uso:
  scala > new Counter(10).incr.decr.inc.inc.contador
 */

class Contador(val cont:Int) {
  def incr():Contador = new Contador(cont+1)
  def decr():Contador = new Contador(cont-1)
}

/*
  Ejercicio 8. Aumentar la clase Counter del anterior ejercicio que permitas al
  usuario opcionalmente pasar un parámetro un Int cómo parámetro da incr y
  decr. Si el parámetro es definido este debe ser un valor por omisión de 1.
 */

class Contador8(val cont:Int) {
  def incr(incremento:Int = 1):Contador8 = new Contador8(cont+incremento)
  def decr(incremento:Int = 1):Contador8 = new Contador8(cont-incremento)
}

/*
  Ejercicio 9. Aumente la clase Contador para adicionar un método que sea llame ajuste.
  Este método debe aceptar un Sumador y retornar un nuevo contador Contador
  con el resultado de aplicar el Sumador a el Contador.
 */
class Sumador(monto:Int){
  def adicionar(valor:Int) = valor + monto
}

class Contador9(val cont:Int) {
  def incr(incremento:Int = 1):Contador9 = new Contador9(cont+incremento)
  def decr(incremento:Int = 1):Contador9 = new Contador9(cont-incremento)
  def ajuste(sum:Sumador):Contador9 = new Contador9(sum.adicionar(cont))
}
