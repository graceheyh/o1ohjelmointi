package o1.blood2

// This code is introduced in Chapter 7.4.

enum Rhesus(val isPositive: Boolean):

  case RhPlus extends Rhesus(true)
  case RhMinus extends Rhesus(false)

  def isNegative = !this.isPositive
  def canDonateTo(recipient: Rhesus) = this.isNegative || this == recipient
  def canReceiveFrom(donor: Rhesus) = donor.canDonateTo(this)
  override def toString = if this.isPositive then "+" else "-"

end Rhesus

// Write your ABO enumeration here.








// This class will work once you have the ABO enum done.
/*
class ABORh(val abo: ABO, val rhesus: Rhesus):
  def canDonateTo(recipient: ABORh) =
    this.abo.canDonateTo(recipient.abo) && this.rhesus.canDonateTo(recipient.rhesus)
  def canReceiveFrom(donor: ABORh) = donor.canDonateTo(this)
  override def toString = this.abo.toString + this.rhesus.toString
*/

