package o1.blood1

class BloodType(val abo: String, val rhesus: Boolean):

  override def toString =
    if rhesus then
      val rh = "+"
      s"${abo}${rh}"
    else
      val rh = "-"
      s"${abo}${rh}"

  def hasSafeABOFor(recipient: BloodType) =
    if this.abo == recipient.abo || this.abo == "O" || ((this.abo == "A" || this.abo == "B") && recipient.abo == "AB") then
      true
    else
      false

  def hasSafeRhesusFor(recipient: BloodType) = 
    if this.rhesus == recipient.rhesus || (this.rhesus == false && recipient.rhesus == true) then
      true
    else
      false  

  def canDonateTo(recipient: BloodType) = hasSafeABOFor(recipient) && hasSafeRhesusFor(recipient)

  def canReceiveFrom(donor: BloodType) = donor.hasSafeABOFor(this) && donor.hasSafeRhesusFor(this)

end BloodType

