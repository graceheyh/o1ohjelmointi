////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it’s not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////

package o1.robots.gui

import o1.robots.RobotBody
import o1.robots.RobotBrain
import o1.util.assignments.*
import o1.util.nice.seq.*

/** The class `RobotType` represents instantiable kinds of robots whose availability
  * the Robots user interface checks dynamically.
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this class works or can be used.** */
private[gui] sealed abstract class RobotType[+BrainClass <: RobotBrain](className: String, additionalParameterTypes: Class[?]*)
  extends DynamicClass[RobotBrain](className, Seq(classOf[String], classOf[RobotBody]) ++ additionalParameterTypes):

  def apply(constructorArguments: Matchable*): Option[BrainClass] =
    Some(this.instantiate(constructorArguments.map(arg)*))

  def instantiateRandom(name: String, body: RobotBody): BrainClass

  override def instantiate(parameters: Argument*): BrainClass =
    super.instantiate(parameters*).asInstanceOf[BrainClass]

end RobotType


/** This companion object of class `RobotType` provides access to a few standard instances of the class.
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this object works or can be used.** */
private[gui] object RobotType:
  private[gui] given CanEqual[RobotType[?], RobotType[?]] = CanEqual.derived

  val All: Seq[RobotType[RobotBrain]] = Seq(Spinbot, Nosebot, Slaybot, Staggerbot, Lovebot)

  trait Basic[+BrainClass <: RobotBrain]:
    self: RobotType[BrainClass] =>
    def instantiateRandom(name: String, body: RobotBody): BrainClass = this.instantiate(arg(name), arg(body))

  case object Spinbot extends RobotType[o1.robots.Spinbot]("o1.robots.Spinbot"), Basic[o1.robots.Spinbot]
  case object Nosebot extends RobotType[o1.robots.Nosebot]("o1.robots.Nosebot"), Basic[o1.robots.Nosebot]
  case object Slaybot extends RobotType[o1.robots.Slaybot]("o1.robots.Slaybot"), Basic[o1.robots.Slaybot]

  case object Staggerbot extends RobotType[o1.robots.Staggerbot]("o1.robots.Staggerbot", classOf[Int]):
    override def instantiateRandom(name: String, body: RobotBody) =
      val randomInt = scala.util.Random.nextInt
      this.instantiate(arg(name), arg(body), arg(randomInt))

  case object Lovebot extends RobotType[o1.robots.Lovebot]("o1.robots.Lovebot", classOf[RobotBody]):
    override def instantiateRandom(name: String, body: RobotBody) =
      val bots = body.world.robotList
      val randomExistingBot = bots.randomElement()
      val newLoveBot = this.instantiate(arg(name), arg(body), arg(randomExistingBot))
      assert(newLoveBot.beloved == randomExistingBot, "The lovebot’s beloved variable was not initialized appropriately.")
      newLoveBot

end RobotType

