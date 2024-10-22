package o1.blood2

// This program is introduced in Chapter 7.4. You don’t have to understand the
// entire code immediately, but once you solve the assignment in the chapter,
// you can run this program to test your solution. It should report on the
// compatibilities between various blood types.

@main def bloodTest() =

  println("Using Rhesus blood types only:")
  for donor <- Rhesus.values; recipient <- Rhesus.values do
    val preamble = s"$donor donating to $recipient".padTo(16, ' ')
    println(preamble + " → " + okOrNot(donor.canDonateTo(recipient)))

  /*
  println("\nUsing ABO blood types only:")
  for donor <- ABO.values; recipient <- ABO.values do
    val preamble = s"$donor donating to $recipient".padTo(18, ' ')
    println(preamble + " → " + okOrNot(donor.canDonateTo(recipient)))

  println("\nUsing ABO and Rhesus in combination:")
  val allABORh = for abo <- ABO.values; rh <- Rhesus.values yield ABORh(abo, rh)
  for donor <- allABORh; recipient <- allABORh do
    val preamble = s"$donor donating to $recipient".padTo(20, ' ')
    println(preamble + " → " + okOrNot(donor.canDonateTo(recipient)))
  */

end bloodTest

private def okOrNot(isOK: Boolean) = if isOK then "OK" else "not OK"