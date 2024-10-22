
package o1.shapes

// This trait is introduced in Chapter 7.3.

trait Shape:
  def isBiggerThan(another: Shape) = this.area > another.area
  def area: Double
  def perimeter: Double


