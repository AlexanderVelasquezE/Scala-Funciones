package example

object Objetos extends App {
  //println(comp.cubo(5.0F))

  //println(comp2.cubo(5L))

  val gato1 = new Gato1("IO","Fawn","Churrus")
  val gato2 = new Gato1("Make","Red","Leche")
  val gato3 = new Gato1("Docker","Blue","Cuido")
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

  val persona1 = Persona("Juan Cardona")
  val persona2 = new Persona("Juan", "Cardona")
  //println(persona1.nombreP())
  //println(persona2.nombreP())
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
class Gato1(val nombre:String,val color:String,val comida:String) {

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

/*
  Ejercicio 10. Implemente un objeto de compañia para la clase Persona que contenga una
  método apply que acepte todo el nombre como una sola cadena en vez de un
  nombre separado por espacios.
 */

class Persona(val nombre:String, val apellido:String) {
  def nombreP:String = s"$nombre $apellido"
}

object Persona {
  def apply(nombre:String):Persona = {
    val arr = nombre.split(" ")
    //val per = new Persona(arr(0),arr(1))
    new Persona(arr(0),arr(1))
    //per
  }
}

/*
  Ejercicio 11. Escriba objetos de compañı́a para las clases Director y Pelicula como sigue:
  Para Director:
    - Un método apply que acepte los mismos parámetros del constructor de la
    clase y retorne un nuevo Director.
    - Un método esMayor que acepte dos Director(es) y retorne el mayor de
    los dos.
  Para Pelicula:
    - Un método apply que acepte los mismos parámetros del constructor de la
    clase y retorne un nuevo Pelicula.
    - Un método mejorCalificada que acepte dos Pelicula(s) y retorna la
    que tiene mayor rangoIMDB entre las dos.
    - Un método mayorDirectorEnElTiempo que acepte dos Pelicula(s) y
    retorne el Director que fue mayor en el momento de presentar la pelı́cula.
 */
class Director (val nombre:String, val apellido:String, val nacimiento:Int){
  def nombreD():String = s"$nombre $apellido"
  def copy (nombre :String=this.nombre, apellido:String=this.apellido,
            nacimiento:Int = nacimiento):Director = new Director(nombre,apellido,nacimiento)
}

object Director {
  def apply(nombre:String, apellido:String, nacimiento:Int) = new Director(nombre,apellido,nacimiento)
  def esMayor(d1:Director,d2:Director):Director = if (d1.nacimiento < d2.nacimiento) d1 else d2
}

class Pelicula (val nombre:String,val presentacion:Int,val rangoIMDB:Double,val director:Director) {
  def directorEdad:Int = presentacion - director.nacimiento

  def esDirigidaPor(director:Director):Boolean = this.director == director

  def copy(nombre:String=this.nombre, presentacion:Int = this.presentacion, rangoIMDB:Double=this.rangoIMDB,
           director:Director=this.director):Pelicula = new Pelicula(nombre,presentacion,rangoIMDB,director)
}

object Pelicula {
  def apply(nombre:String, presentacion:Int, rangoIMDB:Double, director:Director):Pelicula =
    new Pelicula(nombre,presentacion,rangoIMDB,director)

  def mejorCalificada(p1:Pelicula,p2:Pelicula):Pelicula = if(p1.rangoIMDB > p2.rangoIMDB) p1 else p2

  def mayorDirectorEnElTiempo(p1:Pelicula,p2:Pelicula):Director = if(p1.directorEdad > p2.directorEdad) p1.director else p2.director
}