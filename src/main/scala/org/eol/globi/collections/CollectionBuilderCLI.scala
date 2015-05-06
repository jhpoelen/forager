package org.eol.globi.collections

import play.api.libs.json.JsObject

case class Config(verbose: Boolean = false, pageIds: Seq[Long] = Seq())

object CollectionBuilderCLI {

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("scopt") {
      head("foragus", "0.x")
      opt[Unit]("verbose") action { (_, c) =>
        c.copy(verbose = true)
      } text "verbose increases the log levels"
      help("help") text "creates EOL collection json objects for given page ids"
      arg[Long]("eolPageId") maxOccurs 1 minOccurs 1 required() action { (x, c) =>
        c.copy(pageIds = c.pageIds :+ x)
      } text "please specify page id (e.g. 7666 for Phocidae)"
    }

    parser.parse(args, Config()) map { config =>
      val taxonPageId: Long = config.pageIds.head
      val collectionJson: Option[JsObject] = mkEOLCollectionOrNone(taxonPageId)
      println (collectionJson.getOrElse(""))
    }
  }

  def mkEOLCollectionOrNone(taxonPageId: Long): Option[JsObject] = {
    CollectionBuilder.namesForTaxonExternalId(taxonPageId) match {
      case Some((name, commonName)) =>
        Some(mkEOLCollection(taxonPageId, name, commonName))
      case None =>
        None
    }
  }

  def mkEOLCollection(taxonPageId: Long, name: String, commonName: Option[String]): JsObject = {
    val (collectionName, collectionDescription) = CollectionBuilder.mkCollectionInfo(commonName, name, """preysOn""")
    val collectionRef = CollectionBuilder.mkCollectionReference(taxonPageId, name)
    val preyIds: Stream[Long] = CollectionBuilder.preyOf(taxonPageId)
    CollectionBuilder.asEOLCollection(collectionName, collectionRef + "\n" + collectionDescription , preyIds)
  }
}
