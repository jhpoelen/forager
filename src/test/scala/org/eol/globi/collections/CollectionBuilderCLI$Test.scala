package org.eol.globi.collections

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files, Paths}

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.JsObject

import scala.io.Source
import scala.reflect.io.ZipArchive

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
    Files.createDirectories(Paths.get("target/collections/"))
    val files = lines.zipWithIndex.map { case (line, index) => {
      if (index % 10 == 0) println("")
      val pageId: Long = line.trim.toLong
      val col: Option[JsObject] = CollectionBuilderCLI.mkEOLCollectionOrNone(pageId)
      col match {
        case obj: Some[JsObject] =>
          val path: Path = Paths.get("target/collections/" + pageId + ".json")
          Files.write(path, obj.get.toString().getBytes(StandardCharsets.UTF_8))
          print(pageId + " ok. ")
          Some(path.toString)
        case None =>
          print(pageId + " empty. ")
          None
      }
    }
    }
    val archivePath: String = Paths.get("target/collections.zip").toAbsolutePath.toString
    zip(archivePath, files)
    println("collection files archived in [" + archivePath + "].")

    // adapted from http://stackoverflow.com/questions/9985684/how-do-i-archive-multiple-files-into-a-zip-file-using-scala
    def zip(out: String, files: Iterator[Option[String]]) = {
      import java.io.{BufferedInputStream, FileInputStream, FileOutputStream}
      import java.util.zip.{ZipEntry, ZipOutputStream}

      val zip = new ZipOutputStream(new FileOutputStream(out))

      files.foreach {
        case Some(name) =>
          zip.putNextEntry(new ZipEntry(name))
          val in = new BufferedInputStream(new FileInputStream(name))
          var b = in.read()
          while (b > -1) {
            zip.write(b)
            b = in.read()
          }
          in.close()
          zip.closeEntry()
        case None =>
      }
      zip.close()
    }
  }


}