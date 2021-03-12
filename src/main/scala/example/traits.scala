package example

/*
Ejercicio 1. Vamos a modelar un comportamiento común a gatos, leones, ti-
gres, panteras, jaguares, etc. Vamos a definir una interface común (trait) Felino.
En la figura 1. Defina el trait Felino y las diferentes sub-especies de la super
clase ella.
Tenga en cuenta que:
  En Felino se encuentra el color.
  En Felino se encuentra el sonido que hace cada uno de los diferentes tipos.
  Únicamente Gato tiene una comida favorita.
  Un León tiene un tamaño de melena.
  Recuerde que todos los atributos tiene sus correspondientes métodos get-
  ters.
  Los constructores se hacen en las clases no en los traits.
 */
trait Felino {
  def color():String = "Amarillo mostaza"
  def sonido():String
}

final class Leon(val melena:Int = 0) extends Felino {
  override def color():String = "Blanco"
  def sonido():String = "chsrrrz"
}
final class Tigre extends Felino {
  override def color():String =  "Negro"
  def sonido():String = "plop"
}

final class Jaguar extends Felino {
  override def color():String = "B"
  def sonido():String = "blublublu"
}

final class Gato(val comida:String = "Lasagna") extends Felino {
  override def color():String = "salmon"
  def sonido():String = "Miau"
}

/*
Ejercicio 2. Define un trait llamado Forma y tiene tres métodos abstractos:
  tamaño que retorna el número de lados.
  perı́metro que retorna la longitud total de sus lados.
  área que retorna el área de la forma en cuestión.
Implemente Forma con tres clases: Cı́rculo, Rectángulo y Cuadrado. En
cada caso implemente los tres métodos. Asegure un constructor principal con los
parámetros para cada forma y que los campos son accesibles (getters).
 */

sealed trait Forma {
  def tamaño():Int
  def perimetro():Double
  def area():Double
}

trait Rectangular extends Forma {
  override def tamaño(): Int = 4
}

final case class Circulo(val radio:Double = 1) extends Forma {
  def tamaño(): Int = 0
  def perimetro():Double = 2*math.Pi*radio
  def area():Double = math.Pi*radio*radio
  override def toString():String = s"Un circulo de radio $radio" + "cm"
}
/*
Ejercicio 3. La solución del ejercicio anterior produjo tres tipos diferentes de
formas. Sin embargo, este no modela la relación entre los elementos correcta-
mente. Un Cuadrado no es solamente una forma también es una forma de tipo
Rectángulo donde la longitud y la altura son iguales.
Refactorize la solución del anterior ejercicio ası́ el Cuadrado y Rectángulo
son subtipos de un tipo común llamado Rectangular.
 */
final case class Cuadrado(val lado:Double = 1) extends Rectangular {
  def perimetro():Double = lado * tamaño()
  def area():Double = lado*lado
  override def toString():String = s"Un cuadrado de lado $lado" + "cm"
}
final case class Rectangulo(val lado1:Double = 1,val lado2:Double = 2) extends Rectangular {
  def perimetro():Double = lado1*2 + lado2*2
  def area():Double = lado1*lado2
  override def toString():String = s"Un rectangulo de ancho $lado1"+"cm"+s" y largo $lado2"+"cm"
}

/*
Ejercicio 4. Revisemos la forma implementada en la anterior seccion 2. En
primer lugar haga Forma en un sealed trait. Entonces, escriba un objeto único
(singleton object) llamado Draw con un método apply que tomar una Forma co-
mo argumento y returna un descripción de él en la terminal.
Por ejemplo:
scala> Draw(Circulo(10))
res0: String = Un circulo de radio 10.0cm
scala> Draw(Rectangulo(3,4))
res1: String = Un rectangulo de ancho 3.0cm y largo 4.0cm
Finalmente, verifique que el compilador se queja cuando se comente una clausu-
la case en el método Draw.
 */
object Draw {
  def apply(forma: Forma):String = forma.toString
  def apply2(form: Forma):String = form match {
    case Circulo(radio) => s"Un circulo de radio " + radio + "cm"
    case Cuadrado(l1) => s"Un rectangulo de ancho $l1"+"cm"
    case Rectangulo(l1,l2) => s"Un rectangulo de ancho $l1"+"cm"+s" y largo $l2"+"cm"
  }
}

/*
Ejercicio 5. Escriba una clase Color
1. Da al color tres propiedades: RGB (Red,Green,Blue).
2. Cree tres colores predefinidos: Rojo, Amarillo y Rosa.
3. Suministre un medio para que la gente produzca sus propios colores per-
sonalizados de Color con sus propios valores de RGB.
Este ejercicio es abierto a interpretación.
 */
class Color(val r:Int,val g:Int,val b:Int)

object Color{
  def apply(r:Int,g:Int,b:Int):Color = new Color(r,g,b)
}

object Rojo extends Color(255,0,0)
object Amarillo extends Color(255,255,0)
object Rosa extends Color(255,0,255)

object Main extends App {
  val tigre1 = new Tigre
  println(tigre1.color())

  val circulo1 = new Circulo(0.5)
  println(circulo1.area())

  val cuadrado1 = new Cuadrado()
  println(cuadrado1.perimetro())

  val rectangulo1 = new Rectangulo()
  println(rectangulo1.perimetro())

  println(Draw.apply2(circulo1))
  println(Draw.apply2(rectangulo1))

  val color1 = Color(2,33,204)
  println(color1.r,color1.g,color1.b)
}
