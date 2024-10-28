package o1.robots

import o1.*

/** The trait `RobotBrain` represents the “brains” (or artificial “intelligence”) of virtual
  * robots that inhabit two-dimensional grid worlds. A robot brain is equipped with an algorithm
  * for determining what a robot should do during its turn in a robot simulation. In other words,
  * a robot brain is capable of controlling the actions of a robot body.
  *
  * Concrete classes that extend this trait need to provide implementations for the abstract
  * `moveBody` method; each such concrete class can represent a new kind of robot behavior.
  * Overriding the `mayMove` method may also be necessary.
  *
  * @param initialName  the name of the robot
  * @param body         the body the robot brain will control */
trait RobotBrain(initialName: String, val body: RobotBody):

  private var nameIfAny: Option[String] = None
  this.name = initialName

  /** Changes the robot’s name to the given one.
    *
    * **Note to students:** In Scala, a method whose name ends in an underscore and an
    * equals sign — like this one’s — can be called using a special syntax. For instance,
    * this method can be called either with the statement `bot.name_=("Suzy")` or simply
    * with an assignment statement: `bot.name = "Suzy"`. You won’t find many uses for this
    * in O1, but it’s nice to know nonetheless. */
  def name_=(newName: String) =
    this.nameIfAny = if newName.trim.isEmpty then None else Some(newName)

  /** Returns the robot’s name. If the name has been set to the empty string
    * or contains only whitespace, returns `"Incognito"` instead. */
  def name = this.nameIfAny.getOrElse("Incognito")


  /** Returns the world that this robot is located in. */
  def world = this.body.world

  /** Returns the location of this robot in its robot world. */
  def location = this.body.location

  /** Returns the direction this robot is facing in. */
  def facing = this.body.facing


  /** Returns the coordinates that point at the square that is immediately in front
    * of this robot. */
  def locationInFront = body.location.neighbor(body.facing)

  /** Returns the square that is immediately in front of this robot. */
  def squareInFront = this.body.neighboringSquare(body.facing)

  /** Returns the brain of the robot immediately in front of this robot. The
    * brain is returned in an `Option` wrapper; `None` is returned if there
    * is no robot in that square or if the robot that is there has no brain. */
  def robotInFront = squareInFront.robot.flatMap(_.brain)


  /** Controls the robot body’s actions for a single turn. If the brain considers
    * the robot to be stuck (see [[isStuck]]), this method does nothing. If not
    * stuck, the brain calls its own [[moveBody]] method, which carries out the
    * actual robot actions. */
  def controlTurn() =
    if !this.isStuck then
      this.moveBody()


  /** Moves the robot: causes it to change its location, turn around, or whatever else the
    * robot does during its turn. What this means in practice depends on the type (subtype)
    * of the robot brain.
    *
    * `moveBody` is called by the brain’s [[controlTurn]] method every time the robot brain
    * gets a turn, unless the robot is stuck (as defined by [[isStuck]]).
    *
    * This method assumes that it is never called if the robot is broken or stuck. */
  def moveBody(): Unit


  /** Checks the square that neighbors this robot in the given direction to see
    * if it contains something that the robot brain considers an obstacle. This method
    * is abstract; different kinds of robot brains will have different definitions of
    * what counts as an obstacle.
    *
    * This default implementation (which may be overriden by subtypes) does not consider
    * anything to be an obstacle. Robots that rely on this default implementation are willing
    * to move in any direction and never consider themselves to be stuck (see [[isStuck]]). */
  def mayMove(direction: CompassDir) = true

  /** Determines whether the robot brain considers the robot to be stuck. A brain considers
    * the robot stuck if and only if all the squares surrounding the robot contain obstacles,
    * as defined by the [[mayMove]] method. Only four squares are considered: the nearest
    * square in each of the four main compass directions. */
  def isStuck = CompassDir.Clockwise.forall(!mayMove(_))


  /** Moves the robot one square forwards, if there is nothing there. If that square
    * isn’t empty, the robot does *not* move, so this method never causes a collision.
    *
    * (Note: This method *only* moves the robot in case the square in front of it is
    * completely empty. This method does not call [[mayMove]] and is not affected by
    * that other method’s implementation in any way.)
    *
    * @return `true` if the move was successful and the robot is now in the next square,
    *         `false` if it was blocked */
  def advanceCarefully() =
    if squareInFront.robot.isEmpty && squareInFront != Wall then
      body.moveTowards(facing)
      true
    else
      false  
     


  /** Returns a textual representation of the robot (which is the robot’s name). */
  override def toString = this.name

end RobotBrain

