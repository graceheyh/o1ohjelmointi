
package o1.shapes

import scala.math.Pi

// This class is introduced in Chapter 7.3.

class Circle(val radius: Double) extends Shape:
  def area = Pi * this.radius * this.radius
  def perimeter = 2 * Pi * this.radius

