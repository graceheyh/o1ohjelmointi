////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it’s not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////
package o1.robots.gui

import o1.util.*
import o1.util.proper.*
import o1.util.assignments.arg

import o1.robots.{RobotWorld,RobotBody,RobotBrain,Square}
import o1.{GridPos,CompassDir,Pic}
import o1.gui.{Escapable,O1WindowDefaults,BasicGridDisplay,Key}
import o1.gui.swingops.*
import o1.gui.Dialog.*
import o1.gui.layout.*

import o1.robots.gui.scenario.Scenario
import RobotType.*

import scala.swing.*
import scala.swing.Separator
import scala.compiletime.uninitialized
import scala.collection.mutable.Buffer
import scala.annotation.targetName
import scala.language.adhocExtensions // enable extension of Swing classes

/** The class `RobotWindow` represents GUI windows that serve as the main
  * window of a [[RobotApp]].
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how
  * this class works or can be used.** */
open class RobotWindow extends MainFrame, Escapable, O1WindowDefaults:

  this.title = "Robots"
  this.location = Point(20, 20)

  protected val worldPane = FlowPanel()
  protected var worldView: RobotsDisplay = uninitialized
  protected val randomizer: scala.util.Random = scala.util.Random()

  this.menuBar = new MenuBar:
    contents += new Menu("Program"):
      mnemonic = Key.P
      contents += MenuItem(Action("Help")( //noinspection ForwardReference
        display("Right-click the map to add robots or walls.\nUse the buttons in lower right corner to make robots act.\nCreate new worlds using the menus.", RelativeTo(worldPane)) ) )
      contents += MenuItem(Action("Quit")( dispose() ))

  protected val nextRobotTurnButton = new Button:
    action = Action("Next robot")( worldView.nextRobotTurn() )
    preferredSize = Dimension(this.preferredSize.width * 2, this.preferredSize.height + 3)
  protected val advanceFullRoundButton = new Button:
    action = Action("Full turn")( worldView.advanceTurns(1) )
    preferredSize = nextRobotTurnButton.preferredSize
  this.defaultButton = advanceFullRoundButton

  val buttonPanel = FlowPanel(nextRobotTurnButton, advanceFullRoundButton)

  this.contents =
    val innerPanel = new EasyPanel:
      placeNW(worldPane,   (0, 0), TwoWide, FillBoth(1, 1), (0, 0, 0, 0))
      placeNE(buttonPanel, (0, 1), OneSlot, NoFill(1, 0),   (0, 0, 0, 0))
    FlowPanel(innerPanel)

  protected def createView(world: RobotWorld): RobotsDisplay =
    RobotsDisplay(this, world)

  def focusButton = this.advanceFullRoundButton

  def displayWorld(world: RobotWorld) =
    this.worldView = this.createView(world)
    this.worldPane.contents.clear()
    this.worldPane.contents += this.worldView
    this.worldView.update()
    this.pack()

  def update(world: RobotWorld) =
    this.updateButtons(world.robotWithNextTurn)

  protected def updateButtons(nextRobot: Option[RobotBody]) =
    val wasEnabled = this.focusButton.enabled
    this.advanceFullRoundButton.enabled = nextRobot.isDefined
    this.nextRobotTurnButton.enabled = nextRobot.isDefined
    this.nextRobotTurnButton.text = "Next robot" + nextRobot.flatMap( _.brain ).map(" (" + _.name + ")").getOrElse("")
    if !wasEnabled then
      this.focusButton.requestFocusInWindow()

  protected open class RobotsDisplay(val parent: RobotWindow, val world: RobotWorld) extends BasicGridDisplay[RobotWorld, Square](world, RobotsDisplay.MaxSquareSize):
    display =>

    override def update() =
      super.update()
      this.parent.update(this.world)

    val popup = SquarePopup()

    override def tooltipFor(square: Square) =
      for body <- square.robot yield tooltipFor(body.brain)

    private def tooltipFor(brain: Option[RobotBrain]): String =
      brain.map(tooltipFor).getOrElse("brainless unrepairable robot body")

    def tooltipFor(brain: RobotBrain): String =
      brain.toString + " the " + brain.getClass.getSimpleName

    def elementClicked(square: Square) =
      for robot <- square.robot do
        robot.takeTurn()

    def advanceTurns(howMany: Int): Unit =
      for repeat <- 1 to howMany do
        this.world.advanceFullRound()
      this.update()

    def nextRobotTurn(): Unit =
      this.world.advanceTurn()
      this.update()

    protected open class SquarePopup extends Popup:
      import PopupAction.*

      class RobotAction(name: String, applies: RobotBody => Boolean)(perform: RobotBody => Unit)
        extends ElementAction(name, _.robot.exists(applies) )( _.robot.foreach(perform) )

      abstract class AbstractAddRobotItem(name: String) extends PopupAction(name):

        def isApplicable(coords: GridPos) = isAvailable && !world(coords).isUnpassable

        val isAvailable: Boolean

        def requestName(): ProperString =
          val nameIfGiven: Option[String] = requestAnyLine("Robot name:", RelativeTo(display))
          Proper.string(nameIfGiven, default = "Incognito".p)

        def requestFacing(): Option[CompassDir] =
          val dirMap: Map[String, CompassDir] = 
            CompassDir.Clockwise.mapGroups( _.toString.toLowerCase )( _.head )
          val randomChoice: (String, CompassDir) = 
            "random" -> CompassDir.Clockwise.randomElement(RobotWindow.this.randomizer)
          val choices: List[String] = 
            randomChoice.head :: dirMap.keys.toList
          for choice <- requestChoice("Initial facing:", choices, RelativeTo(display))
            yield (dirMap + randomChoice)(choice)

        def requestBrain(body: RobotBody): Option[RobotBrain]

        def apply(coords: GridPos) =
          for facing <- this.requestFacing() do
            val noBotThereYet = world(coords).isEmpty
            if noBotThereYet then
              val newRobot: RobotBody = world.addRobot(coords, facing)
              newRobot.brain = this.requestBrain(newRobot)
            else
              val dummyBot = RobotBody(world, coords, facing)
              dummyBot.brain = this.requestBrain(dummyBot)
              world(coords).addRobot(dummyBot)

      end AbstractAddRobotItem

      object AddBrainlessItem extends AbstractAddRobotItem("Add a brainless bot"):
        val isAvailable = true
        def requestBrain(body: RobotBody) = None

      abstract class AddBasicRobotItem(name: String, val robotType: RobotType[RobotBrain]) extends AbstractAddRobotItem(name):

        val isAvailable = this.robotType.isUsable

        def requestBrain(body: RobotBody) =
          val name = requestName()
          for params <- requestParameters(name, body) yield
            this.robotType.instantiate(params.map(arg)*)

        def requestParameters(name: String, body: RobotBody): Option[IndexedSeq[Matchable]]

      end AddBasicRobotItem

      trait Parameterless extends AddBasicRobotItem:
        def requestParameters(name: String, body: RobotBody) = Some(IArray[Matchable](name, body))

      object AddStaggerbotItem extends AddBasicRobotItem("Add a staggerbot", Staggerbot):
        def requestParameters(name: String, body: RobotBody) =
          for seed <- requestAnyInt("Random seed:", "Please enter an integer.", RelativeTo(display))
            yield IArray[Matchable](name, body, seed)

      object AddLovebotItem extends AddBasicRobotItem("Add a lovebot", Lovebot):

        def requestParameters(name: String, body: RobotBody) =
          for beloved <- requestBeloved(body) yield IArray[Matchable](name, body, beloved)

        def requestBeloved(self: RobotBody) =
          val options = world.robotList.filterNot( _ == self ).flatMap( _.brain )
          if options.isEmpty then
            Some(self)
          else if options.sizeIs == 1 then
            options.headOption.map( _.body )
          else
            val chosenBrain = requestChoice("Choose the beloved:", options, RelativeTo(display))
            chosenBrain.map( _.body ) orElse Some(self)

      end AddLovebotItem


      val addRobotItems = Seq[PopupAction](
        new AddBasicRobotItem("Add a spinbot", Spinbot) with Parameterless,
        new AddBasicRobotItem("Add a nosebot", Nosebot) with Parameterless,
        AddLovebotItem,
        AddStaggerbotItem,
        new AddBasicRobotItem("Add a slaybot", Slaybot) with Parameterless,
        AddBrainlessItem)
      val addWallItem  = PosAction("Add a wall", world(_).isEmpty )(world.addWall)
      val takeTurnItem = RobotAction("Play turn", AlwaysApplicable)( _.takeTurn() )
      val fixItem      = RobotAction("Fix", robot => !robot.isIntact )( _.fix() )
      val destroyItem  = RobotAction("Destroy", _.isIntact )( _.destroy() )

      this += addWallItem
      this ++= addRobotItems
      this += Separator()
      this += takeTurnItem
      this += fixItem
      this += destroyItem

    end SquarePopup

    val wallPic       = this.getPic("wall",   true)
    val floorPic      = this.getPic("floor",  true)
    val brokenPic     = this.getPic("broken", true)
    val directionPics = CompassDir.Clockwise.mapTo( dir => this.getPic("arrow_" + dir, true) )
    val robotPics     = loadRobotPics(Seq("Nosebot", "Lovebot", "Slaybot", "Staggerbot", "Spinbot"))

    def robotPic(robot: RobotBody) = this.robotPics(robot.brain.map( _.getClass.getCanonicalName ).getOrElse(""))

    def getPic(name: String, fullSize: Boolean): Option[BufferedImage] =
      Pic.asImage("pictures/" + name.toLowerCase + ".png").map( this.scalePic(_, fullSize) )

    def scalePic(image: BufferedImage, fullSize: Boolean) =
      this.scale(image, if fullSize then this.squareSize else this.squareSize * 9 / 10)

    def loadRobotPics(types: Iterable[String]) =
      types.mapify( "o1.robots." + _ )( this.getPic(_, false) ) withDefaultValue this.getPic("unknown", false)

    def missingElementVisuals = Array.empty
    def elementVisuals(square: Square): Array[BufferedImage] =
      val terrainPic   = if square.isUnpassable then this.wallPic else this.floorPic
      val statusPic    = if square.robot.exists( bot => !bot.isIntact ) then this.brokenPic else None
      val robotPic     = square.robot.flatMap(this.robotPic)
      val directionPic = square.robot.flatMap( robot => this.directionPics(robot.facing) )
      Array(terrainPic, robotPic, directionPic, statusPic).map( _.orNull )

  end RobotsDisplay

  private object RobotsDisplay:
    val MaxSquareSize = 60

end RobotWindow


/** The trait `RobotScenarios` can be mixed into a `RobotWindow` to provide a selection of ready-made scenarios in a GUI menu.
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this trait works or can be used.** */
private[gui] trait RobotScenarios:
  scenarios: RobotWindow =>

  private final val basicScenarioMenu = new ScenarioMenu("World"):
    this ++= Scenario.AllStandard.map( ScenarioItem(_) )
    mnemonic = Key.W

  this.menuBar.contents += basicScenarioMenu

  protected def scenarioMenus = Seq(this.basicScenarioMenu)

  // this is used to hack around a possible scala Swing bug (?): a menu’s contents buffer always shows up as empty
  protected class ScenarioMenu(name: String) extends Menu(name):
    val items = Buffer[ScenarioItem]()
    @targetName("addAll")
    def ++=(items: Iterable[ScenarioItem]) =
      this.contents ++= items
      this.items ++= items

  protected class ScenarioItem(val scenario: Scenario) extends MenuItem(scenario.name):
    this.action = new Action(scenario.name):
      this.accelerator = scenario.accelerator
      def apply() =
        for world <- scenario.setup(scenarios.worldPane) do
          scenarios.displayWorld(world)
        for menu <- scenarioMenus; item <- menu.items do
          item.enabled = item.scenario.isAvailable
    this.enabled = scenario.isAvailable

end RobotScenarios

