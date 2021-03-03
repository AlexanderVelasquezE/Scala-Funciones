package example

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

object Draw {
  def apply(forma: Forma):String = forma.toString
  def apply2(form: Forma):String = form match {
    case Circulo(radio) => s"Un circulo de radio " + radio + "cm"
    case Cuadrado(l1) => s"Un rectangulo de ancho $l1"+"cm"
    case Rectangulo(l1,l2) => s"Un rectangulo de ancho $l1"+"cm"+s" y largo $l2"+"cm"
  }
}

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
