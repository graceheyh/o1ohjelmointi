package o1.election

import scala.collection.mutable.Buffer           // This is useful in early versions of the class.
import scala.math.Ordering.Double.TotalOrdering  // This will be useful in later assignments.

// Write your code here.
class District(val name: String, val seats: Int, val candidates: Vector[Candidate]):

  override def toString: String = s"$name: ${candidates.size} candidates, $seats seats"

  def printCandidates() =
    candidates.foreach((can: Candidate) => println(can.toString))

  def candidatesFrom(party: String) =
    val parcan = Buffer[Candidate]()
    candidates.filter(_.party == party)

  def topCandidate =
    if candidates.nonEmpty then
      var topSoFar = candidates.head
      for can <- candidates.tail do
        if can.votes > topSoFar.votes then
          topSoFar = can
      Some(topSoFar)
    else
      None

  def totalVotes =
    candidates.foldLeft(0)(_ + _.votes)

  def totalVotes(party: String) =
    candidatesFrom(party).foldLeft(0)(_ + _.votes)
