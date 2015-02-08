package org.eol.globi.collections

import org.scalatest._
import play.api.libs.json.{JsObject, JsString}

import scala.io.Source

class CollectionBuilder$Test extends FlatSpec with Matchers {

  "A scientific taxon name" should "be transformed with english suffices" in {
    CollectionBuilder.commonize("Phocidae") should be("Phocid")
    CollectionBuilder.commonize("Arthropoda") should be("Arthropoda")
    CollectionBuilder.commonize("somethingElse") should be("somethingElse")
  }

  "A list of scientific names" should "be transformed into" in {
    val lines: Iterator[String] = Source.fromURL(getClass.getResource("/name-test.csv")).getLines()
    lines.drop(1) foreach (line => {
      val row = line.split(",")
      CollectionBuilder.commonize(row(0)) should be(row(1))
    })
  }

  "retrieve taxon info" should "include common name" in {
    val rez: Option[(String, String)] = CollectionBuilder.namesForTaxonExternalId( """7666""")
    rez should be(Some( """Phocidae""", """true seals"""))
  }

  "retrieve taxon info" should "include common name no match" in {
    val rez: Option[(String, String)] = CollectionBuilder.namesForTaxonExternalId( """xx7666""")
    rez should be(None)
  }

  "create a description" should "include a human readable text" in {
    val commonName = """true seals"""
    val scientificName = """Phocidae"""
    val interactionType = """preysOn"""

    val (collectionName, collectionDescription) = CollectionBuilder.mkCollectionInfo(commonName, scientificName, interactionType)

    collectionName should be( """True Seals Food""")
    collectionDescription should be( """what do true seals eat?
what do phocids eat?
what do phocidae eat?
what do true seals prey on?
what do phocids prey on?
what do phocidae prey on?
what do true seals hunt?
what do phocids hunt?
what do phocidae hunt?
true seals prey
phocid prey
phocidae prey
true seals food
phocid food
phocidae food""")
  }

  "a lucene query" should "be nicely created" in {
    val luceneQuery: String = CollectionBuilder.buildLucenePathQuery(Seq("327955", "7666"))
    luceneQuery should be( """'path:EOL\\:327955 OR path:EOL\\:7666'""")

  }

  "a query against remote neo4j" should "return something" in {
    val preyIds: Stream[String] = CollectionBuilder.preyOf("""327955""")
    preyIds should contain( """2849458""")

    val collectionName = """collectionName"""
    val collectionDescription = """collectionDescription"""
    val eolCollection: JsObject = CollectionBuilder.asEOLCollection(collectionName, collectionDescription, preyIds)
    eolCollection \\ "collection_item_id" should contain(JsString("2849458"))
  }

}