package o1.city

import o1.*
import scala.util.Random

/** A `Simulator` object is a city simulator based on Schelling’s model of emergent
  * social segregation. Its key methods are `startNew`, which launches a new simulation,
  * and `moveResidents`, which advances the most recently launched simulation by moving
  * dissatisfied residents into vacant homes. A selection of other methods is also
  * provided for examining the state of the simulation.
  *
  * As implied above, a `Simulator` is mutable: Calling `startNew` causes the
  * simulator to discard any previously active simulation and start processing
  * a new one. Calling `moveResidents` modifies the state of the active simulation. */
class Simulator:

  // State of the active (latest) simulation:
  private var cityMap = CityMap(1, Vector()) // most-recent holder; this initial value will be replaced by startNew
  private var similarityDesired = 0.0        // most-recent holder; this initial value will be replaced by startNew


  /** Launches a new simulation. The simulation will take place on a newly
    * initialized city grid as specified by the parameters provided. After calling
    * this method, [[moveResidents]] will advance the newly launched simulation;
    * any previously ongoing simulation is simply discarded.
    * @param squaresPerSide     the number of residences in each row and column (the city grid is always square)
    * @param fillPercent        the percentage of residences that have a resident and are not vacant (between 0 and 100)
    * @param demographics       the demographics that populate the non-vacant residences; the first element of the
    *                           collection is the “main” demographic
    * @param occupiedRatio      the percentage of non-vacant residences occupied by the “main” demographic (between
    *                           0 and 100); the remaining residences are evenly split between the other demographics
    * @param similarityDesired  the minimum percentage of neighboring residences that must match a household’s
    *                           demographic in order for the household to be satisfied (between 0 and 100) */
  def startNew(squaresPerSide: Int, fillPercent: Int, demographics: Vector[Occupied], occupiedRatio: Int, similarityDesired: Int) =
    import o1.util.nice.number.*
    val residents = fillPercent * (squaresPerSide * squaresPerSide) / 100
    val reds      = (occupiedRatio atLeast 0 atMost 100) * residents / 100
    val others    = (residents - reds) / (demographics.size - 1)
    val allCounts = reds +: Vector.fill(demographics.size - 1)(others)
    this.cityMap = CityMap(squaresPerSide, demographics zip allCounts)
    this.similarityDesired = similarityDesired atLeast 0 atMost 100



  /** Returns all the locations in the city that contain the given demographic. */
  def findDemographic(demographic: Demographic): Vector[GridPos] =
    cityMap(


  /** Returns all the locations in the city being simulated. */
  def allAddresses = this.cityMap.allAddresses


  /** Returns the locations of all dissatisfied households in the city. A household is
    * dissatisfied if it does not have a sufficient proportion of neighbors that share
    * its demographic. For example, if the city-dwellers desire a similarity of 50%, at
    * least four of a household’s eight neighbors need to share its demographic or it will
    * be dissatisfied. (Dissatisfied residents will move when [[moveResidents]] is next called.)
    *
    * When determining a household’s neighbors, diagonally adjacent grid locations count,
    * and many households will have a full eight neighbors. However, `Vacant` locations
    * don’t count as neighbors, nor do locations that are beyond the edges of the grid.
    * A corner location, then, will only have a maximum of three neighbors, less if any of
    * the three locations are vacant.
    *
    * For example, consider a non-vacant corner location. There are three locations next to it
    * on the grid. If one of them shares the corner’s demographic, another doesn’t, and the
    * third one is vacant (and therefore ignored), then 50% of the corner’s two neighbors
    * are similar to the corner.
    *
    * Vacant locations are never returned by this method (i.e., they count as satisfied).
    * Any household with zero neighbors is considered to be satisfied. */
  def dissatisfiedResidents: Vector[GridPos] =
    ??? // TODO: implementation missing


  /** Advances the most recently launched simulation by one step, moving all dissatisfied
    * households to vacant locations on the grid. During the simulation step, the dissatisfied
    * households move one at a time, in random order, each receiving a random home among the
    * ones that are vacant at that moment. (That is, a household may move to a home vacated
    * by another dissatisfied household earlier during the same simulation step.)
    *
    * Satisfied residents stay where they are. If a satisfied resident becomes dissatisfied
    * during this simulation step (because others move), that does not cause them to move (yet).
    *
    * *Example:* Suppose X and Y are residents that neighbor each other. Further suppose
    * that X is satisfied and Y is dissatisfied when `moveResidents` is called to advance
    * the simulation by one step. This means that Y will move to a random location. X, on the
    * other hand, will not move during that `moveResidents` call, no matter what happens in
    * X’s neighborhood. X will only move the next time `moveResidents` is called, in case X
    * is dissatisfied at that time.
    *
    * If there are no vacant addresses at all in the city, this method does nothing.
    *
    * @see [[dissatisfiedResidents]]
    * @see [[startNew]] */
  def moveResidents() =
    val vacantAddresses = this.findDemographic(Vacant).toBuffer
    if vacantAddresses.nonEmpty then
      DoNothing // TODO: replace this with a proper implementation


  /** Returns a `Map` whose keys are all the demographics present in the city ([[Vacant]] included)
    * and whose values list all the locations where that demographic is present.
    *
    * **Note to students: This method will be implemented only in Chapter 10.1.**
    * You won’t need it before that. */
  def residents: Map[Demographic, Vector[GridPos]] =
    ??? // TODO: implementation missing


  /** Returns the proportion of satisfied residents in the city. For example, a return value
    * of 0.0 means that nobody is satisfied, 1.0 that everyone is, and 0.95 that most are.
    * @see [[dissatisfiedResidents]] */
  def satisfactionLevel =
    1.0 - this.dissatisfiedResidents.size.toDouble / this.cityMap.size


  /** Returns the city currently being simulated, as a [[CityMap]]. */
  def currentMap = this.cityMap

end Simulator


/* Note to students: This object enables you to set the simulator’s random seed, which
   is something you may want to do while debugging your program, as mentioned in the
   ebook chapter. There is no need to touch this code otherwise. */
private[o1] object Simulator:

  // Feel free to uncomment the line below and use whichever seed number you like.
  def withCustomSeed() =
    // Random.setSeed(123456)
    Simulator()

end Simulator

