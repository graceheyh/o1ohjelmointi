package o1.blood1

/** This program goes through all the possible combinations of donors and recipients in the
  * ABO+Rh system and produces a printout detailing which combinators are safe. To determine
  * whether a combination is safe, the app uses class `BloodType`.
  *
  * The output is expected to match [[https://en.wikipedia.org/wiki/Blood_type#Red_blood_cell_compatibility
  * the one provided by Wikipedia]]:
  *
  * Note to students: You don’t need to understand how this app works just yet.
  * You can run it to test your `BloodType` class, though. */
@main def bloodTest() =
  for
    donor <- allTypes
    recipient <- allTypes
  do
    reportOn(donor, recipient)

/** A collection of all the blood types. */
private val allTypes =
  for
    abo <- Seq("A","B","AB","O")
    rh  <- Seq(true, false)
  yield BloodType(abo, rh)

/** Prints out a report of the compatibility of two blood types. */
private def reportOn(donor: BloodType, recipient: BloodType) =
  def okOrNot(isOK: Boolean) = if isOK then "OK    " else "not OK"
  val canDonate  = okOrNot(donor.canDonateTo(recipient))
  val canReceive = okOrNot(recipient.canReceiveFrom(donor))
  val preamble1 =     donor.padLeft + " donating to "    + recipient.padRight
  val preamble2 = recipient.padLeft + " receiving from " + donor.padRight
  println(s"$preamble1 → $canDonate          $preamble2 → $canReceive")

/** Utilities for text formatting. */
extension (btype: BloodType)
  private def padRight =
    val btypeName: String = btype.toString
    btypeName.padTo(width, ' ')
  private def padLeft =
    val missing = width - s"$btype".length
    if missing > 0 then " " * missing + btype else s"$btype"

private val width: Int =
  val btypeNames: Seq[String] = allTypes.map( _.toString )
  btypeNames.maxBy( _.length ).length

