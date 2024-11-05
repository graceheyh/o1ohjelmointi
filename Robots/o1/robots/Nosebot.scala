package o1.robots

import o1.*

// TODO: proper implementation missing completely
class Nosebot(name: String, body: RobotBody) extends RobotBrain(name, body):

  override def mayMove(direction: CompassDir): Boolean =
    body.neighboringSquare(direction).isEmpty

  def moveBody() = LazyList.continually(this.attemptMove()).find(_ == true)
  // TODO: missing methods

  def attemptMove() =
    if this.advanceCarefully() then
      true
    else
      body.spinClockwise()
      false

end Nosebot



