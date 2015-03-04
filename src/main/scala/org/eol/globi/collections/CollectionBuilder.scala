package org.eol.globi.collections

import org.anormcypher.{Cypher, Neo4jREST}
import play.api.libs.json.{JsObject, Json, JsArray}

object CollectionBuilder {

  def namesForTaxonExternalId(taxonId: String): Option[(String, Option[String])] = {
    val language: String = """@en"""
    val query = Cypher(
      """START taxon = node:taxons(externalId='EOL:""" + taxonId +
        """')
          | RETURN taxon.name as name, taxon.commonNames? as commonNames""".stripMargin)

    val rez = query.apply().map(row => {
      val commonNameList: Option[String] = row[Option[String]]("commonNames")
      val commonName = firstEnglishCommonName(commonNameList)
      val name: String = row[String]("name")
      (name, commonName)
    }
    )
    if (rez.isEmpty) None else Some(rez.head)
  }

  def firstEnglishCommonName(commonNameList: Option[String]): Option[String] = {
    parseCommonNames(commonNameList).flatten.filter(_._2 == "en") map {
      _._1
    } headOption
  }

  def parseCommonNames(commonNamesString: Option[String]): Array[Option[(String, String)]] = {
    val names: Array[String] = commonNamesString.getOrElse("").split( """\s+\|\s+""")
    val pattern = """(.*)@(\w\w)""".r
    names map {
      case pattern(name, lang) => Some(name.trim, lang)
      case _ => None
    }
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

  def mkCollectionReference(id: String, name: String): String = {
    List( """This collection was automatically generated from <a href="http://globalbioticinteractions.org">Global Biotic Interactions</a> (GloBI) data. Please visit <a href="""",
      """http://eol.org/pages/""",
      id,
      """/data">this EOL data page</a> for more detailed information about the GloBI interaction data and to find other trait data for """,
      name,
      """."""
    ).mkString("")
  }

  def mkCollectionInfo(commonName: Option[String], scientificName: String, interactionType: String): (String, String) = {
    val interactionTargetTitle = Map("preysOn" -> "Food")
    val interactionTargetNouns = Map("preysOn" -> List("prey", "food"))
    val interactionVerbs = Map("preysOn" -> List("eat", "prey on", "hunt"))

    val pluralCommonNames = List(commonName).flatten.map {
      name => if (name.endsWith("s")) name else name + "s"
    }
    val pluralNames = pluralCommonNames ++ commonizePlural(scientificName)
    val singularNames = List(commonName).flatten ++ commonize(scientificName)
    val sentences = interactionVerbs(interactionType).map {
      verb => pluralNames.map { name => "what do " + name.toLowerCase + " " + verb + "?"}
    }.flatten
    val phrases = interactionTargetNouns(interactionType).map {
      noun => singularNames.map { name => name.toLowerCase + " " + noun}
    }.flatten

    val name: String = List(commonName, Some(scientificName)).flatten.head.split(" ").map(_.capitalize).mkString(" ") + " " + interactionTargetTitle(interactionType)
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
