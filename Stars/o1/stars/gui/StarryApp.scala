package o1.stars.gui

import o1.gui.{Pos, Tooltips}
import o1.gui.mutable.View
import o1.stars.{StarMap, StarCoords}
import o1.stars.files.readStarMap

/** Launches a GUI that presents the user with a map of the night sky.
  *
  * The app uses `readStarMap` to read the star map from a local folder.
  * It then uses `createSkyPic` to turn that data into a [[o1.gui.Pic Pic]]. */
@main def runStarryApp() =
  readStarMap(dataFolderPath) match
    case Some(starMap) => this.showInGUI(starMap)
    case None          => println(s"Failed to access star data in folder $dataFolderPath.")

private val starDataFolder = "test" // You can use either "northern" or "test" here.

private val dataFolderPath = s"o1/stars/$starDataFolder/"

private def showInGUI(sky: StarMap) =

  object starView extends View(sky, tickRate=0, title="Stars"), Tooltips.Everpresent:

    val Size = 650
    val starrySky = createSkyPic(sky, Size)
    def makePic = this.starrySky

    override def onMouseMove(mousePosition: Pos) =
      val mouseCoord = StarCoords.fromPercentages(mousePosition.x / Size, mousePosition.y / Size)
      val nearbyConstellations = sky.constellations.filter( _.isNearish(mouseCoord) )
      val constellationNames = nearbyConstellations.map( _.name ).mkString(", ")
      this.tooltip = s"$mouseCoord $constellationNames"

  end starView

  starView.start()

end showInGUI

