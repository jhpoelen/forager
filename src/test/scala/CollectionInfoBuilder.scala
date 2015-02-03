import org.scalatest._

import scala.io.Source

class CollectionInfoBuilder extends FlatSpec with Matchers {

  "A scientific taxon name" should "be transformed with english suffices" in {
    commonize("Phocidae") should be("Phocid")
    commonize("Arthropoda") should be("Arthropoda")
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
    collectionInfo("description") should be( """what do true seals eat?
what do phocids eat?
what do phocidae eat?
what do true seals prey on?
what do phocids prey on?
what do phocidae prey on?
what do true seals hunt?
what do phocids hunt?
what do phocidae hunt?
true seal prey
phocid prey
phocidae prey
true seal food
phocid food
phocidae food""")
  }

  def mkCollectionInfo(commonName: String, scientificName: String, interactionType: String): Map[String, String] = {
    val interactionTargetTitle = Map("preysOn" -> "Food")
    val interactionTargetNouns = Map("preysOn" -> List("prey", "food"))
    val interactionVerbs = Map("preysOn" -> List("eat", "prey on", "hunt"))

    val pluralNames = List(commonName+ "s", commonize(scientificName) + "s", scientificName)
    val singularNames = List(commonName, commonize(scientificName), scientificName)
    val sentences = interactionVerbs(interactionType).map { verb => pluralNames.map { name => "what do " + name.toLowerCase  + " " + verb + "?" } }.flatten
    val phrases = interactionTargetNouns(interactionType).map { noun => singularNames.map { name => name.toLowerCase + " " + noun } }.flatten

    Map("description" -> (sentences ++ phrases).mkString("\n")
      , "title" -> (commonName.split(" ").map(_.capitalize).mkString(" ") + " " + interactionTargetTitle(interactionType)))
  }

  def commonize(name: String): String = {
    val idae = """(.*)(idae)$""".r
    name match {
      case idae(prefix, suffix) => prefix + "id"
      case _ => name
    }
  }
}