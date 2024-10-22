package o1.medical

// This code is explained in Chapter 7.3.

trait MedicalPro(val employer: String)

class Paramedic(city: String, val inAmbulance: Boolean) extends MedicalPro(city)

trait Doctor extends MedicalPro

class GeneralPractitioner(employer: String) extends MedicalPro(employer), Doctor

trait Specialist(val specialization: String) extends Doctor

class Neurologist(employer: String) extends MedicalPro(employer), Specialist("neurology")
