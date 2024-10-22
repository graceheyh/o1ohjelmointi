package o1.shapes

// This class is introduced in Chapter 7.3.

import scala.collection.mutable.Buffer

@main def shapeTest() =

  val shapes = Buffer(Circle(10), Rectangle(10, 100), Circle(5))

  var sumOfAreas = 0.0
  for current <- shapes do
    sumOfAreas += current.area
  println("The sum of the areas is: " + sumOfAreas)

end shapeTest

