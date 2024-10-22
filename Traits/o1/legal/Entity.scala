package o1.legal

// TODO: define trait Entity properly
trait Entity(val name: String):
  def contact: NaturalPerson
  def kind:String

  override def toString: String = s"${name} (${kind})"
  
  
