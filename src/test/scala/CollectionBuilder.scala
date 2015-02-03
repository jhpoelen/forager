import org.anormcypher._
import org.scalatest._
import play.api.libs.json._

import scala.io.Source

class CollectionBuilder extends FlatSpec with Matchers {

  "a query against remote neo4j" should "return something" in {
    val taxonConceptIds: Iterator[String] = Source.fromURL(getClass.getResource("collection-taxa.csv")).getLines()
    val ids = taxonConceptIds.toList
    val luceneQuery: String = buildLucenePathQuery(ids)
    luceneQuery should be( """'path:EOL\\:327955 OR path:EOL\\:7666'""")

    val stream: Stream[(Seq[String], Seq[String])] = preyOf(luceneQuery)

    val result = stream.toList

    val humanOnly = result.filter(row => row._1.contains( """327955"""))

    humanOnly.head._1 should contain( """327955""")
    humanOnly.head._2 should contain( """2849458""")

    val phocidaeOnly = result.filter(row => row._1.contains( """7666"""))

    phocidaeOnly.head._1 should contain( """7666""")
    phocidaeOnly.head._2 should contain( """1905""")

    val predAndTheirPrey = ids.map(id => {
      (id, result.filter(row => row._1.contains(id)).map(_._2).flatten)
    })

    predAndTheirPrey map (_._1) should contain( """327955""")
    predAndTheirPrey map (_._1) should contain( """7666""")

    val collectionName = """collectionName"""
    val collectionDescription = """collectionDescription"""


    val collectionList = predAndTheirPrey map (predAndPrey => {
      Json.obj(
        "collection" -> Json.obj("name" -> predAndPrey._1
          , "description" -> "collection description")
        , "collection_items" -> Json.arr(
          predAndPrey._2 map (preyId => {
            Json.obj(
              "collection_item_id" -> preyId,
              "collection_item_type" -> "TaxonConcept",
              "sort_field" -> "12"
            )
          })
        )
      )
    })

    Json.arr(collectionList) \\ "collection_item_id" should contain(JsString("2849458"))
  }

  def preyOf(luceneQuery: String): Stream[(Seq[String], Seq[String])] = {
    implicit val connection = Neo4jREST("api.globalbioticinteractions.org", 7474, "/db/data/")
    val query = Cypher(
      """START taxon = node:taxonPaths(""" + luceneQuery +
        """)
          |MATCH taxon-[:ATE|PREYS_ON]->otherTaxon
          | WHERE has(taxon.pathIds) AND has(otherTaxon.externalId) AND otherTaxon.externalId =~ 'EOL:.*'
          | RETURN replace(taxon.pathIds, "EOL:", "") as predatorPathIds, collect(replace(otherTaxon.externalId, "EOL:", "")) as preyIds""".stripMargin)
    val result = query.apply().map(row => {
      val predIds: List[String] = row[String]("predatorPathIds").split( """\|""").map(id => id.trim).toList
      val preyIds: List[String] = row[Seq[String]]("preyIds").toList
      predIds -> preyIds
    }
    )
    result
  }

  def buildLucenePathQuery(taxonConceptIds: Seq[String]): String = {
    val
    luceneQuery =
      taxonConceptIds.map(id => """path:EOL\\:""" + id).mkString("'", " OR ", "'")
    luceneQuery
  }
}