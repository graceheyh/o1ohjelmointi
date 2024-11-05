package o1.people

class Passenger(val name: String, val card: Option[TravelCard]):
  
  def canTravel =
    card match
      case Some(kortti) => if kortti.isValid then true else false
      case None => false
      
  def hasCard = 
    card match
      case Some(kortti) => true
      case None => false
        





