////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it’s not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////

package o1.city.gui

import o1.city.*
import o1.util.isImplemented
import o1.util.nice.number.*
import o1.gui.*
import o1.gui.swingops.*
import o1.grid.GridPos
import o1.gui.View.HasPauseToggle
import o1.gui.mutable.*
import o1.gui.layout.*
import scala.swing.*
import scala.swing.event.ButtonClicked
import javax.swing.SwingUtilities.invokeLater
import scala.language.adhocExtensions // enable extension of Swing classes

/** The singleton object `SimulatorApp` represents an application that runs
  * simulations based on Schelling’s model of emergent social segregation. The
  * application appears in a GUI window and provides controls for adjusting
  * simulation settings.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this object works
  * on the inside. It’s enough to know that you can use this file to start the program.** */
object SimulatorApp extends App:

  val simulator = Simulator.withCustomSeed()
  val supportsMultipleDemographics = isImplemented(simulator.residents)

  private val AllDemographics = ColorScheme.DemographicColors.map( Occupied(_) )
  private def residentsOfTwoDGs = AllDemographics.take(2).map( dg => (dg, simulator.findDemographic(dg)) )
  val PixelsPerSquare = 20
  val SizeInSquares = 40
  val WindowSizeInPixels = 600

  val gui = new SimpleFrame("City Simulation") with O1WindowDefaults with Escapable with TerminatesOnClose:

    // For now, this draws faster than a prettier makePic-based solution.
    // The code is messy due in part to CityMap’s current interface (which is what it is for pedagogical reasons).
    private val screen = new ViewComponentWithCustomRendering(simulator, WindowSizeInPixels, WindowSizeInPixels) with HasPauseToggle:
      override def startsPaused = true

      override def render(myGraphics: Graphics2D): Unit =
        val cityMap = simulator.currentMap
        val squareInPixels = WindowSizeInPixels / cityMap.homesPerSide
        val vacantSquare: BufferedImage =
          val vacantPic = Pic.square(squareInPixels, o1.White)
          vacantPic.toImage

        class GraphicScheme(demographic: Demographic):

          val (satisfiedImage, dissatisfiedImage): (BufferedImage, BufferedImage) =
            demographic match
              case Vacant => (vacantSquare, vacantSquare)
              case occupied: Occupied =>
                val unhappinessMarker: Pic =
                  val markerLight = if occupied.label.intensity > 50 then 50 else 200
                  val markerColor = Color(markerLight, markerLight, markerLight, 100)
                  Pic.circle((squareInPixels / 2) atLeast 3, markerColor)
                val satisfiedPic = Pic.square(squareInPixels, ColorScheme(demographic))
                val dissatisfiedPic = unhappinessMarker onto satisfiedPic
                (satisfiedPic.toImage, dissatisfiedPic.toImage)

          def image(isSatisfied: Boolean) =
            if isSatisfied then this.satisfiedImage else this.dissatisfiedImage

        end GraphicScheme

        val schemes = collection.mutable.Map[Demographic, GraphicScheme]()

        class SquareStatus(val demographic: Demographic, var isSatisfied: Boolean):
          def this(x: Int, y: Int) = this(cityMap(GridPos(x, y)), true)
          def image: BufferedImage =
            val scheme = schemes.getOrElseUpdate(this.demographic, GraphicScheme(this.demographic))
            scheme.image(this.isSatisfied)

        val statuses = IArray.tabulate(cityMap.homesPerSide, cityMap.homesPerSide)( SquareStatus(_, _) )
        for loc <- simulator.dissatisfiedResidents do
          statuses(loc.x)(loc.y).isSatisfied = false

        for x <- 0 until cityMap.width; y <- 0 until cityMap.height do
          statuses(x)(y).image.draw(myGraphics, x = x * squareInPixels, y = y * squareInPixels)

      end render

      override def onTick() =
        stepForward()

    end screen

    val stepButton  = new Button("Single Step") { preferredSize = preferredSize.withWidth(100) }
    val runButton   = new Button("Run")         { preferredSize = preferredSize.withWidth(100) }
    val startButton = new Button("Start Over")  { preferredSize = preferredSize.withWidth(100) }
    val buttonRow   = FlowPanel(stepButton, runButton, startButton)
    this.listenTo(stepButton, runButton, startButton)
    this.reactions += {
      case ButtonClicked(`startButton`) => startNewSimulation()
      case ButtonClicked(`stepButton`)  => stepForward(); screen.refresh()
      case ButtonClicked(`runButton`)   => togglePause()
    }


    private def stepForward() =
      invokeLater( () =>
        simulator.moveResidents()
        satisfactionPercentLabel.text = satisfactionText
      )


    private def startNewSimulation() =
      simulator.startNew(sizeSlider.value, 100 - vacancySlider.value,
                         AllDemographics.take(populationCountSlider.value),
                         proportionSlider.value, similaritySlider.value)
      satisfactionPercentLabel.text = satisfactionText
      screen.refresh()


    trait RestartsSim extends Setting:
      override def onAdjust() =
        super.onAdjust()
        startNewSimulation()

    def satisfactionText = "Satisfied: " + "%.1f".format(simulator.satisfactionLevel * 100) + "%"
    val satisfactionPercentLabel = Label()
    val similaritySlider: Setting      = new Setting("Threshold:",              0,  70, 100, _.toString + "% similarity desired." ) with RestartsSim
    val populationCountSlider: Setting = new Setting("Number of demographics:", 2,  2,   10, n => s"$n populations.") with RestartsSim:
      this.enabled = supportsMultipleDemographics
      override def onAdjust() =
        super.onAdjust()
        proportionSlider.updateLabel()

    def nonRedDescription: String = if populationCountSlider.value > 2 then "others" else "Blue"
    val proportionSlider     = new Setting("Relative sizes:", 0,  50,  100, ratio   => s"$ratio% Red, ${100 - ratio}% $nonRedDescription.") with RestartsSim
    val vacancySlider        = new Setting("Vacancy:",        0,  10,  100, vacancy => s"$vacancy% vacant locations." ) with RestartsSim
    val sizeSlider           = new Setting("Grid size:",      1,  20,  100, side    => s"$side by $side squares.") with RestartsSim
    val speedSlider: Setting = new Setting("Speed:",        -85,   0,  130, value   => "Trying for ~" + "%.1f".format(tickRate(0.95 * value)) + " steps/s."):
      override def onAdjust() =
        updateSpeed()

    private def tickRate(sliderValue: Double) = math.pow(10, sliderValue / 100.0)
    private def updateSpeed() =
      screen.adjustSpeed(tickRate(this.speedSlider.value))

    contents = new EasyPanel:
      placeN(screen,                   (0, 0), TwoWide, FillBoth(1, 1),    (2, 2, 2, 2))
      placeN(satisfactionPercentLabel, (0, 1), TwoWide, Slight,            NoBorder)
      placeN(buttonRow,                (0, 2), TwoWide, Slight,            NoBorder)
      placeNW(similaritySlider,        (0, 3), OneSlot, FillHorizontal(1), NoBorder)
      placeNW(vacancySlider,           (0, 4), OneSlot, FillHorizontal(1), NoBorder)
      placeNW(speedSlider,             (0, 5), OneSlot, FillHorizontal(1), NoBorder)
      placeNE(populationCountSlider,   (1, 3), OneSlot, FillHorizontal(1), NoBorder)
      placeNE(proportionSlider,        (1, 4), OneSlot, FillHorizontal(1), NoBorder)
      placeNE(sizeSlider,              (1, 5), OneSlot, FillHorizontal(1), NoBorder)

    this.updateSpeed()
    startNewSimulation()
    screen.start()
    this.pack()

    private def togglePause() =
      screen.togglePause()
      runButton.text = if screen.isPaused then "Run" else "Pause"
      stepButton.enabled = screen.isPaused

    override def closeOperation() =
      screen.stop()
      super.closeOperation()

  end gui

  gui.visible = true

end SimulatorApp
