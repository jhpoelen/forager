import collection.mutable.Stack
import org.scalatest._

import scala.io.Source

class CommonizerSpec extends FlatSpec with Matchers {

  "A scientific taxon name" should "be transformed with english suffices" in {
    commonize("Phocidae") should be("phocid")
    commonize("Arthropoda") should be("arthropod")
    commonize("somethingElse") should be("somethingElse")
  }

  "A list of scientific names" should "be transformed into" in {
    val lines: Iterator[String] = Source.fromURL(getClass.getResource("name-test.csv")).getLines()
    lines.drop(1) foreach (line => {
      val row = line.split(",")
      commonize(row(0)) should be(row(1))
    })
  }

  def commonize(name: String): String = {
    val idae = """(.*)(idae)$""".r
    val oda = """(.*)(oda)$""".r
    name match {
      case idae(prefix, suffix) => prefix.toLowerCase + "id"
      case oda(prefix, suffix) => prefix.toLowerCase + "od"
      case _ => name
    }
  }
}