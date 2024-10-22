package o1.election
import scala.annotation.targetName

/** Represents a candidate in an election results application.
  *
  * A candidate object is immutable.
  *
  * @param name   the candidate’s name
  * @param votes  the number of votes received by the candidate
  * @param party  the name (or abbreviation) of the candidate’s party */
class Candidate(val name: String, val votes: Int, val party: String):

  /** Returns a short textual description of the candidate. */
  override def toString = s"$name ($party): $votes"

  /** Determines if the candidate has more votes than the other, given candidate. */
  @targetName("hasMoreVotesThan")
  infix def >(another: Candidate) = this.votes > another.votes
  // Side note to students: the infix modifier, above, indicates that the method is intended to
  // be used as an infix “operator”. That is, myCand > another rather than myCand.>(another)
  // Since the method has a symbolic, non-verbal name, it is nice to give it a targetName
  // annotation. More information: https://dotty.epfl.ch/docs/reference/other-new-features/targetName.html

end Candidate

