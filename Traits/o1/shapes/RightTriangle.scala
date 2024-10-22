package o1.shapes
import scala.math.sqrt

// TODO: define RightTriangle properly
class RightTriangle(val sideLength: Double, val anotherSideLength: Double) extends Shape:
  def hypotenuse = sqrt(sideLength * sideLength + anotherSideLength * anotherSideLength)
  def area = sideLength * anotherSideLength / 2
  def perimeter = sideLength + anotherSideLength + this.hypotenuse