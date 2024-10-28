package o1.robots

import o1.*

/** The class `Spinbot` represents the “brains” of extremely simple robots
  * that simply spin clockwise and never change location.
  * @param name  the name of the spinbot
  * @param body  the robot body whose actions the spinbot brain will control */
class Spinbot(name: String, body: RobotBody) extends RobotBrain(name, body):

  /** Moves the robot. A spinbot simply spins 90 degrees clockwise as its move. */
  def moveBody() =
    body.spinClockwise()

end Spinbot