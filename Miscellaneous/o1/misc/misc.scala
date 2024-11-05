package o1.misc
import o1.*

// Various small assignments across several chapters will ask you to define functions in this file.

def together(laulut: Vector[String], tahti: Int) =
  laulut.mkString("&") + "/" + tahti

def tempo(laulu: String) =
  if laulu.contains('/') then
    val pilko = laulu.split("/")
    pilko(1).toInt
  else
    120
  


def myFunc = ???