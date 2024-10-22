package o1.cruising
import scala.collection.mutable.Buffer

@main def driveAbout() =
  val car = Car()
  car.receivePassenger(Schoolkid())
  car.receivePassenger(ChemicalEngineer("C. Chemist"))
  car.receivePassenger(MechanicalEngineer("M. Machine"))
  car.receivePassenger(ElectricalEngineer("E. Electra"))
  car.receivePassenger(ComputerScientist("C.S. Student"))
  car.start()


class Car:
  private val passengers = Buffer[Passenger]()

  def receivePassenger(passenger: Passenger) =
    passenger.sitDown()
    this.passengers += passenger

  def start() =
    println("(The car won't start.)")
    for passenger <- this.passengers do
      passenger.remark()
end Car


trait Passenger(val name: String):
  def sitDown() =
    println(this.name + " finds a seat.")

  def speak(sentence: String) =
    println(this.name + ": " + sentence)

  def diagnosis: String

  def remark() =
    this.speak(this.diagnosis)
end Passenger


trait Student extends Passenger:
  def diagnosis = "No clue what's wrong."

class Schoolkid extends Passenger("Anonymous pupil"), Student

trait TechStudent extends Student:
  override def remark() =
    super.remark()
    this.speak("Clear as day.")

class ChemicalEngineer(name: String) extends TechStudent, Passenger(name):
  override def diagnosis = "It's the wrong octane. Next time, I'll do the refueling."

class MechanicalEngineer(name: String) extends TechStudent, Passenger(name):
  override def diagnosis = "Nothing wrong with the gas. It must be the pistons."
  override def speak(sentence: String) =
    super.speak(sentence.replace(".", "!"))

class ElectricalEngineer(name: String) extends TechStudent, Passenger(name):
  override def sitDown() =
    println(this.name + " claims a front seat.")
  override def diagnosis = "Hogwash. The spark plugs are faulty."

class ComputerScientist(name: String) extends TechStudent, Passenger(name):
  override def remark() =
    this.speak(super.diagnosis)
    this.speak(this.diagnosis)
  override def diagnosis = "Let's all get out of the car, close the doors, reopen, and try again."

