package o1.items
import scala.collection.mutable.Buffer

// TODO: Complete this as instructed in Chapter 7.5.

class Container(name: String) extends Item(name):

  private val contains = Buffer[Item]()

  def addContent(newContent: Item): Unit =
    contains += newContent

  def contents: Vector[Item] = contains.toVector

  override def toString: String = s"${super.toString} containing ${contains.size} item(s)"


end Container

