////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it’s not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////

package o1.robots.gui.scenario

import o1.robots.{RobotWorld,RobotBrain,RobotBody}
import o1.robots.gui.*
import RobotType.*
import o1.gui.Dialog.*
import scala.swing.*
import o1.*
import o1.util.nice.seq.*
import javax.swing.KeyStroke

/** This class represents ready-made robot world scenarios (that the user can choose
  * to load up via the Scenario menu).
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this class
  * works or can be used.**
  *
  * @param name  the name of the scenario, to be displayed in the menu */
abstract class Scenario(val name: String):

  def accelerator: Option[KeyStroke] = None
  def setup(locator: Component): Option[RobotWorld]
  def isAvailable: Boolean

  protected def requestParameter(prompt: String, min: Int, locator: Component) =
    requestInt(prompt, _ >= min, s"Please enter an integer no lower than $min.", RelativeTo(locator))
  protected def requestWidth     (locator: Component) = requestParameter("Width of the world:", 1, locator)
  protected def requestHeight    (locator: Component) = requestParameter("Height of the world:", 1, locator)
  protected def requestWallCount (locator: Component) = requestParameter("Number of walls:", 0, locator)
  protected def requestRobotCount(locator: Component) = requestParameter("Number of robots:", 0, locator)

end Scenario


object Scenario:
  val AllStandard = Seq( EmptyWorld, Noses, Bar, Verona, Termination, OneOfEach,
                         RandomDefault("Random (default, small)", 10),
                         RandomDefault("Random (default, big)", 40),
                         RandomCustom("Random (custom)", RobotType.All),
                         RandomCustom("Random (custom, no slaybots)", RobotType.All.filterNot( _ == Slaybot )) )
end Scenario


object EmptyWorld extends Scenario("Empty (custom size)"):

  def isAvailable = true

  def setup(locator: Component) =
    for
      width <- this.requestWidth(locator)
      height <- this.requestHeight(locator)
    yield RobotWorld(width, height)

end EmptyWorld


trait FromMap extends Scenario:

  protected def map: Seq[String]

  protected def addBots(world: RobotWorld): Unit

  private def addWalls(world: RobotWorld) =
    val wallLocs = for
      y <- this.map.indices
      x <- this.map(y).indices
      if this.map(y)(x) == '#'
    yield GridPos(x + 1, y + 1)
    wallLocs.foreach(world.addWall)

  protected def addBot(world: RobotWorld, x: Int, y: Int, facing: CompassDir) =
    world.addRobot(GridPos(x + 1, y + 1), facing)

  def setup(locator: Component) =
    val world = RobotWorld(this.map.head.length, this.map.size)
    this.addWalls(world)
    this.addBots(world)
    Some(world)

end FromMap


object Noses extends Scenario("Noses"), FromMap:

  def isAvailable = Nosebot.isUsable

  protected val map = Seq("...#......",
                          ".#.....#..",
                          "..........",
                          "...####...",
                          "..........",
                          ".#........")

  protected def addBots(world: RobotWorld) =
    val gogol     = this.addBot(world, 9, 5, South)
    val pinocchio = this.addBot(world, 3, 2, West)
    val cyrano    = this.addBot(world, 4, 0, West)
    val kleopatra = this.addBot(world, 3, 5, North)
    val malden    = this.addBot(world, 7, 3, East)
    gogol.brain     = Spinbot("Gogol", gogol)
    pinocchio.brain = Nosebot("Pinocchio", pinocchio)
    cyrano.brain    = Nosebot("Cyrano", cyrano)
    kleopatra.brain = Nosebot("Kleopatra", kleopatra)
    malden.brain    = Nosebot("Malden", malden)

end Noses


object Bar extends Scenario("Bar"), FromMap:

  def isAvailable = Staggerbot.isUsable && Nosebot.isUsable

  protected val map = Seq("..#......#..",
                          "..########..",
                          "............",
                          "........##..",
                          ".##.....##..",
                          ".##.....##..",
                          ".##.........",
                          ".....###....",
                          ".....###....",
                          "............")

  protected def addBots(world: RobotWorld) =
    val tender = this.addBot(world, 4, 0, South)
    tender.brain = Nosebot("Tender", tender)
    val flyLocs = for
      row <- this.map.indices
      col <- this.map(row).indices
      if row != 0 || col < 2 || col > 9
      coords = GridPos(col + 1, row + 1)
      if world(coords).isEmpty && (row + col) % 3 != 2
    yield coords
    var flyCount = 0
    for loc <- flyLocs do
      flyCount += 1
      val fly = world.addRobot(loc, CompassDir.Clockwise(flyCount % 4))
      fly.brain = Staggerbot("Fly " + flyCount, fly, Integer.valueOf(flyCount))

end Bar


object Verona extends Scenario("Verona"), FromMap:

  def isAvailable = Staggerbot.isUsable && Lovebot.isUsable

  protected val map = Seq("........##.##.",
                          ".......#######",
                          ".......#######",
                          "........#####.",
                          ".........###..",
                          "..........#...",
                          "..............",
                          "..##.##.......",
                          ".#######......",
                          ".#######......",
                          "..#####.......",
                          "...###........",
                          "....#.........")

  protected def addBots(world: RobotWorld) =
    val romeo  = this.addBot(world, 12, 9, West)
    val juliet = this.addBot(world, 0, 0, East)
    romeo.brain  = Staggerbot("Romeo", romeo, Integer.valueOf(4))
    juliet.brain = Lovebot("Juliet", juliet, romeo)

end Verona


object Termination extends Scenario("Termination"), FromMap:

  def isAvailable = Slaybot.isUsable && Nosebot.isUsable

  protected val map = Seq("...#......",
                          ".#.....#..",
                          "..........",
                          "...#####..",
                          "..........",
                          ".#........")

  protected def addBots(world: RobotWorld) =
    val tutuntuntunen = "(d)-----------------------" * 2 + "&P:<<" + "(dC)(dC) (dC) (dC)(dC)     " * 2 + "(DCCCCe<E>)(DCCCCe<E>) (DCCCCe<E>) (DCCCCe<E>)(DCCCCe<E>)     " * 2
    o1.play(s"[40]<<<$tutuntuntunen/180")
    val arska   = this.addBot(world, 3, 2, West)
    val snap    = this.addBot(world, 4, 0, West)
    val crackle = this.addBot(world, 3, 5, North)
    val pop     = this.addBot(world, 0, 3, East)
    arska.brain   = Slaybot("Arska", arska)
    snap.brain    = Nosebot("Snap!", snap)
    crackle.brain = Nosebot("Crackle!", crackle)
    pop.brain     = Nosebot("Pop!", pop)

end Termination


object OneOfEach extends Scenario("One of each"), FromMap:

  def isAvailable = Nosebot.isUsable && Staggerbot.isUsable && Lovebot.isUsable && Slaybot.isUsable

  protected val map = Seq("#................",
                          "######........###",
                          "#....#........#..",
                          "#...###......###.",
                          "#....#........#..",
                          "#................",
                          "......#..........",
                          ".##..#######...#.",
                          ".##...#....######",
                          "...........#...#.")

  protected def addBots(world: RobotWorld) =
    val romu   = this.addBot(world, 12, 1, West)
    val niina  = this.addBot(world, 5, 8, North)
    val pertsa = this.addBot(world, 2, 2, North)
    val lauri  = this.addBot(world, 16, 0, South)
    val daniel = this.addBot(world, 9, 5, South)
    romu.brain   = Spinbot("Romu", romu)
    niina.brain  = Nosebot("Niina", niina)
    pertsa.brain = Slaybot("Pertsa", pertsa)
    lauri.brain  = Lovebot("Lauri", lauri, niina)
    daniel.brain = Staggerbot("Daniel", daniel, Integer.valueOf(133))

end OneOfEach


trait Randomized extends Scenario:
  val randomizer: scala.util.Random
  def makeDirection() = CompassDir.Clockwise.randomElement(this.randomizer)

trait UsesDefaultRandom extends Randomized:
  val randomizer = scala.util.Random


trait RandomBasic(val acceptedTypes: Seq[RobotType[RobotBrain]]) extends Randomized:

  def isAvailable = this.acceptedTypes.exists( _.isUsable )

  protected def generateWorld(width: Int, height: Int, walls: Int, robots: Int) =
    val world = RobotWorld(width, height)
    val free = for y <- 0 until height; x <- 0 until width yield GridPos(x + 1, y + 1)
    val targets = this.randomizer.shuffle(free).take(walls + robots)
    val (wallLocs, robotLocs) = targets.splitAt(walls)
    wallLocs.foreach(world.addWall)
    val bodies = robotLocs.map( world.addRobot(_, this.makeDirection() ))
    for (body, index) <- bodies.zipWithIndex do
      body.brain = Some(this.makeBrain(body, index + 1))
    world

  def makeBrain(body: RobotBody, robotNumber: Int) =
    val name = "Random " + robotNumber
    val types = this.acceptedTypes.filter( _.isUsable )
    val randomType = types.randomElement(this.randomizer)
    randomType.instantiateRandom(name, body)

end RandomBasic


final class RandomCustom(name: String, acceptedTypes: Seq[RobotType[RobotBrain]])
      extends Scenario(name), RandomBasic(acceptedTypes), UsesDefaultRandom:
  def setup(locator: Component) =
    for
      width  <- this.requestWidth(locator)
      height <- this.requestHeight(locator)
      walls  <- this.requestWallCount(locator)
      robots <- this.requestRobotCount(locator)
    yield this.generateWorld(width, height, walls, robots)


final class RandomDefault(name: String, size: Int)
      extends Scenario(name), RandomBasic(RobotType.All.filterNot( _ == Slaybot )), UsesDefaultRandom:
  def setup(locator: Component) =
    Some(this.generateWorld(size, size, (size * size) / 25, (size * size) / 6))

