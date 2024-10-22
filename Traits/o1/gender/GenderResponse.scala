package o1.gender

// You’ll complete this code in a Chapter 7.4 assignment.

sealed trait GenderResponse  // TODO: seal this

sealed trait Selected(val chosenLabel: String) extends GenderResponse  // TODO: seal this too

object Male extends Selected("male")
object Female extends Selected("female")
object NonBinary extends Selected("non-binary")
// TODO: add a NonBinary object with the text "non-binary"

class Specified(val description: String) extends GenderResponse// TODO: should be a subtype of GenderResponse

// TODO: add a PreferNotToSay object
object PreferNotToSay extends GenderResponse


// Once you’ve solved the assignment by filling in the above code, you may want to try
// uncommenting the test function below. On rebuilding the code, the Scala compiler will
// issue a warning that the match command is not exhaustive. And indeed, the command doesn’t
// cover all cases of the GenderResponse hierarchy and the warning is apt: this code will
// crash at runtime. The compiler is able to issue such warnings for sealed traits.


@main def test() =
  for response <- Vector(NonBinary, Male, PreferNotToSay, Female, Specified("agender")) do
    response match
      case selectedGender: Selected =>
        println(s"selected option: ${selectedGender.chosenLabel}")
      case specifiedGender: Specified =>
        println(s"self-described: ${specifiedGender.description}")


