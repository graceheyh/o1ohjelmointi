package o1.stars

import o1.*

/** Each `Star` object represents a star on a star map. It records some basic information
  * (an identifier, a location, and a magnitude); in addition, some but not all stars have
  * been assigned a name. (In reality, many stars have multiple alternative names, but this
  * class does not capture that fact.)
  *
  * @param id         a number that uniquely identifies the star from the other visible stars (a so-called Henry Draper number)
  * @param coords     the location of the star on a two-dimensional star map
  * @param magnitude  the apparent magnitude (brightness) of the star: *smaller means brighter!*
  * @param name       the star’s name (wrapped in `Some`), or `None` */
class Star(val id: Int, val coords: StarCoords, val magnitude: Double, val name: Option[String]):

  // TODO: add the missing methods
  def posIn(skyPic: Pic): Pos = coords.toImagePos(skyPic)

  override def toString =
    name match
      case Some(name) => s"#$id $name (${coords.toString})"
      case None => s"#$id (${coords.toString})"

  
end Star

