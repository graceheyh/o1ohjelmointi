package o1.service

// The NationalService example is explained in Chapter 7.4. Note that the
// version in the chapter differs somewhat from the code below. The version
// below further incorporates the optional material from the chapter: the
// "case" keyword has been introduced to assist in pattern matching.
//
// The chapter’s other suggestion of making the classes final
// (i.e., uninheritable) has also been followed.


sealed trait NationalService

sealed trait Military extends NationalService:
  val branch: String
final case class Armed(val branch: String) extends Military
final case class Unarmed(val branch: String) extends Military

final case class Civilian(val position: String) extends NationalService

sealed trait HasNotServed extends NationalService
final case class Exempted(val grounds: String) extends HasNotServed
case object Unassigned extends HasNotServed



@main def testDescriptions() =
  val examples = Vector(Armed("navy"), Unassigned, Civilian("Aalto"), Exempted("health"), Unarmed("army"))
  examples.map(describeService).foreach(println)

// The match command below exploits case classes, whose contents are easy to “extract”.
def describeService(service: NationalService): String =
  service match
    case Armed(branch)      => s"armed service in the $branch"
    case Unarmed(branch)    => s"unarmed service in the $branch"
    case Civilian(position) => s"civilian service at $position"
    case Exempted(reason)   => s"exempted on grounds of $reason"
    case Unassigned         => "has not been assigned to service (yet at least)"

