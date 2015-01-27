import org.scalatest._

import scala.io.Source

class CollectionInfoBuilder extends FlatSpec with Matchers {

  "A scientific taxon name" should "be transformed with english suffices" in {
    commonize("Phocidae") should be("Phocid")
    commonize("Arthropoda") should be("Arthropod")
    commonize("somethingElse") should be("somethingElse")
  }

  "A list of scientific names" should "be transformed into" in {
    val lines: Iterator[String] = Source.fromURL(getClass.getResource("name-test.csv")).getLines()
    lines.drop(1) foreach (line => {
      val row = line.split(",")
      commonize(row(0)) should be(row(1))
    })
  }

  "create a description" should "include a human readable text" in {
    val commonName = """true seal"""
    val scientificName = """Phocidae"""
    val interactionType = """preysOn"""

    val collectionInfo: Map[String, String] = mkCollectionInfo(commonName, scientificName, interactionType)

    collectionInfo("title") should be( """True Seal Food""")
    collectionInfo("description") should be("what do true seals eat?\n" +
      "what do Phocids eat?\n" +
      "what do true seals prey on?\n" +
      "what do Phocids prey on?\n" +
      "what do true seals hunt?\n" +
      "what do Phocids hunt?\n" +
      "true seal prey\n" +
      "phocid prey\n" +
      "true seal food\n" +
      "phocid food\n")
  }

  def mkCollectionInfo(commonName: String, scientificName: String, interactionType: String): Map[String, String] = {
    val interactionTargetTitle = Map("preysOn" -> "Food")
    val interactionTargetNouns = Map("preysOn" -> List("prey", "food"))
    val interactionVerbs = Map("preysOn" -> List("eat", "prey on", "hunt"))

    val sentences = interactionVerbs(interactionType).map { verb => "what do " + commonName + "s " + verb + "?\nwhat do " + commonize(scientificName) + "s " + verb + "?\n"}
    val phrases = interactionTargetNouns(interactionType).map { noun => commonName + " " + noun + "\n" + commonize(scientificName).toLowerCase + " " + noun + "\n"}

    Map("description" -> (sentences.reduce(_ + _) + phrases.reduce(_ + _))
      , "title" -> (commonName.split(" ").map(_.capitalize).mkString(" ") + " " + interactionTargetTitle(interactionType)))
  }

  def commonize(name: String): String = {
    val idae = """(.*)(idae)$""".r
    val oda = """(.*)(oda)$""".r
    val ora = """(.*)(ora)$""".r
    name match {
      case idae(prefix, suffix) => prefix + "id"
      case oda(prefix, suffix) => prefix + "od"
      case ora(prefix, suffix) => prefix + "ore"
      case _ => name
    }
  }
}