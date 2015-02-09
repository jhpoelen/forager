package org.eol.globi.collections

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files, Paths}

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.JsObject

import scala.io.Source

class CollectionBuilderCLI$Test extends FlatSpec with Matchers {

  "calling commandline tool" should "return something" in {
    val res = CollectionBuilderCLI.main(Array("one", "two"))
    res should be(())
  }

  "calling commandline tool with invalid taxon page id" should "return something useful" in {
    val res = CollectionBuilderCLI.main(Array("one"))
    res should be(())
  }

  "calling commandline tool" should "return something useful" in {
    val res = CollectionBuilderCLI.main(Array("34543"))
    res should be(())
  }

  "calling commandline tool without ids" should "say something" in {
    val res = CollectionBuilderCLI.main(Array())
    res should be(())
  }

  "calling commandline tool" should "using provided eol id list" in {
    val lines: Iterator[String] = Source.fromURL(getClass.getResource("/popular_eol_page_ids.csv")).getLines()
    lines.foreach(line => {
      val pageId: String = line.trim
      val col: Option[JsObject] = CollectionBuilderCLI.mkEOLCollectionOrNone(pageId)
      col match {
        case obj:Some[JsObject] =>
          val path: Path = Paths.get("target/" + pageId + ".json")
          Files.write(path, obj.get.toString().getBytes(StandardCharsets.UTF_8))
          println("wrote [" + path.toAbsolutePath + "]")
        case None =>

      }
    }
    )
  }


}