package o1.robots

/** The trait `Square` represents squares in a robot world, as an abstract concept.
  * A square object is potentially mutable: its state can change as robots enter and exit it.
  * Two concrete kinds of squares have been implemented: `Floor`s and `Wall`s.
  * @see [[Floor]]
  * @see [[Wall]] */
trait Square:

  /** Returns `true` if the square is completely empty (of robots and walls), `false` otherwise. */
  def isEmpty: Boolean

  /** Returns `true` if the square contains any robot or wall, `false` otherwise. */
  def nonEmpty: Boolean = !this.isEmpty

  /** Returns the robot occupying the square, wrapped in an `Option`, or `None`,
    * if there is no robot in the square. */
  def robot: Option[RobotBody]

  /** Returns `true` if the square contains a permanent, unpassable barrier, `false` otherwise. */
  def isUnpassable: Boolean

  /** Removes any robot from the square (if there was one there to begin with). */
  def clear(): Unit

  /** Adds the given robot to the square, if possible. If there is something
    * already in the square, a collision happens instead.
    * @param arrivee  the robot arriving in the square
    * @return `true` if `arrivee` was successfully placed in the square, `false` if a collision occurred */
  def addRobot(arrivee: RobotBody): Boolean

end Square


/** The singleton object `Wall` represents walls, that is, squares that constitute unpassable
  * barriers for robots. A robot can never be in the same location with a wall.
  *
  * Since all wall locations in all robot worlds are alike and immutable, it is enough to
  * have a single `Wall` object that can be placed anywhere in any robot world. There is no
  * need for separate instances for each wall square. */
object Wall extends Square:

  /** `None` since there is never any robot in a wall square */
  val robot = None

  /** `false` since there is a wall */
  val isEmpty = false

  /** `true` since a wall is an unpassable barrier */
  val isUnpassable = true

  /** Fails to add the given robot to the square. Instead, the robot collides
    * with the wall and is destroyed.
    * @param arrivee  the robot attempting to arrive in the square
    * @return `false` since the robot’s arrival failed */
  def addRobot(arrivee: RobotBody) =
    // TODO: implementation partially missing
    false

  /** Does nothing, since there is no robot to remove in a wall square. */
  def clear() =
    () // no actions necessary

end Wall


/** The class `Floor` represents squares that robots can successfully occupy and move through.
  * On creation, a floor square is always empty, but this changes if a robot arrives. */
class Floor extends Square:

  private var occupant: Option[RobotBody] = None

  /** `false` since a floor is never considered entirely unpassable */
  val isUnpassable = false

  /** Returns the robot occupying the square, wrapped in an `Option`, or `None`,
    * if there is no robot in the square. */
  def robot = this.occupant

  /** Returns `true` if the floor square has no robot in it, `false` otherwise. */
  def isEmpty = occupant == None

  /** Removes any robot from the square (if there was one there to begin with). */
  def clear() =
    this.occupant = None

  /** Adds the given robot in the floor square, if possible. The arrival fails
    * if the square already had an occupant. In that case, the occupant gets
    * destroyed; the robot that crashed into it stays intact and does not get
    * added in the square.
    *
    * (This method never affects the contents of any other square besides the
    * single floor square on which the method is invoked.)
    *
    * @param arrivee  the robot attempting to arrive in the square
    * @return `true` if `arrivee` was successfully placed in the square, `false` if a collision occurred */
  def addRobot(arrivee: RobotBody) =
    this.occupant = Some(arrivee)  // TODO: replace with a full implementation
    true

end Floor

