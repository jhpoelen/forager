import collection.mutable.Stack
import org.scalatest._

import scala.io.Source

class EnglisherSpec extends FlatSpec with Matchers {

  "A scientific taxon name" should "be transformed with english suffices" in {
    englisher("Phocidae") should be("phocid")
    englisher("Arthropoda") should be("arthropod")
    englisher("somethingElse") should be("somethingElse")
  }

  "A list of scientific names" should "be transformed into" in {
    val lines: Iterator[String] = Source.fromURL(getClass.getResource("name-test.csv")).getLines()
    val assertions = lines.drop(1) foreach (line => {
      val row = line.split(",")
      englisher(row(0)) should be(row(1))
    })
  }

  def englisher(name: String): String = {
    val idae = """(.*)(idae)$""".r
    val oda = """(.*)(oda)$""".r
    name match {
      case idae(prefix, suffix) => prefix.toLowerCase + "id"
      case oda(prefix, suffix) => prefix.toLowerCase + "od"
      case _ => name
    }
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}