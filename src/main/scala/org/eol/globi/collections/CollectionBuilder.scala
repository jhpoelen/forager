package org.eol.globi.collections

import org.anormcypher.{Cypher, Neo4jREST}
import play.api.libs.json.{JsObject, Json, JsArray}

object CollectionBuilder {

  def namesForTaxonExternalId(taxonId: String): Option[(String, String)] = {
    val language: String = """@en"""
    val query = Cypher(
      """START taxon = node:taxons(externalId='EOL:""" + taxonId +
        """')
          | RETURN taxon.name as name, taxon.commonNames? as commonNames""".stripMargin)

    val rez = query.apply().map(row => {
      val commonName: String = row[String]("commonNames")
        .split( """\|""").filter(_.contains(language))
        .map(name => name.replaceAll(language, """""").trim).head
      val name: String = row[String]("name")
      (name, commonName)
    }
    )
    if (rez.isEmpty) None else Some(rez.head)
  }

  implicit def connection: Neo4jREST = {
    Neo4jREST("api.globalbioticinteractions.org", 7474, "/db/data/")
  }

  def preyOf(id: String): Stream[String] = {
    val query = Cypher(
      """START taxon = node:taxonPaths(""" + buildLucenePathQuery(Seq(id)) +
        """)
          |MATCH taxon-[:ATE|PREYS_ON]->otherTaxon
          | WHERE has(otherTaxon.externalId) AND otherTaxon.externalId =~ 'EOL:.*'
          | RETURN replace(otherTaxon.externalId, "EOL:", "") as preyId""".stripMargin)
    query.apply().map(row => {
      row[String]("preyId")
    })
  }


  def buildLucenePathQuery(taxonConceptIds: Seq[String]): String = {
    val luceneQuery =
      taxonConceptIds.map(id => """path:EOL\\:""" + id).mkString("'", " OR ", "'")
    luceneQuery
  }

  def asEOLCollection(name: String, description: String, preyIds: Seq[String]): JsObject = {
    Json.obj("name" -> name
      , "description" -> description
      , "collection_items" -> Json.arr(
        preyIds map (preyId => {
          Json.obj(
            "collection_item_id" -> preyId,
            "collection_item_type" -> "TaxonConcept"
          )
        })
      )
    )
  }

  def mkCollectionInfo(commonName: String, scientificName: String, interactionType: String): (String, String) = {
    val interactionTargetTitle = Map("preysOn" -> "Food")
    val interactionTargetNouns = Map("preysOn" -> List("prey", "food"))
    val interactionVerbs = Map("preysOn" -> List("eat", "prey on", "hunt"))

    val pluralNames = List(if (commonName.endsWith("s")) commonName else commonName + "s") ++ commonizePlural(scientificName)
    val singularNames = List(commonName) ++ commonize(scientificName)
    val sentences = interactionVerbs(interactionType).map {
      verb => pluralNames.map { name => "what do " + name.toLowerCase + " " + verb + "?"}}.flatten
    val phrases = interactionTargetNouns(interactionType).map {
      noun => singularNames.map { name => name.toLowerCase + " " + noun}}.flatten

    val name: String = commonName.split(" ").map(_.capitalize).mkString(" ") + " " + interactionTargetTitle(interactionType)
    val description: String = (sentences ++ phrases).mkString("\n")
    (name, description)
  }

  def commonize(name: String): List[String] = {
    val idae = """(.*)(idae)$""".r
    name match {
      case idae(prefix, suffix) => List(prefix + "id", name)
      case _ => List(name)
    }
  }

  def commonizePlural(name: String): List[String] = {
    val idae = """(.*)(idae)$""".r
    name match {
      case idae(prefix, suffix) => List(prefix + "ids", name)
      case _ => List(name)
    }
  }


}
