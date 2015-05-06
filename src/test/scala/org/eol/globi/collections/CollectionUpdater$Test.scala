package org.eol.globi.collections

import dispatch._, Defaults._
import org.scalatest._
import play.api.libs.json.{JsValue, Json, JsObject, JsString}

import scala.io.Source

class CollectionUpdater$Test extends FlatSpec with Matchers {

  def authHeader = Map("Authorization" -> """Token token="fa1fe503868a7390988770f13e6ade5398f15563""")

  //"a collection" should "be created, updated and deleted" in {
  ignore should "be created, updated and deleted" in {
    val eolCollectionJson = CollectionBuilder.asEOLCollection("my cool collection", "my test description", Seq(1L, 208553L))

    val urlString: String = "http://bocce.eol.org/wapi/collections"
    println("creating collection ...")
    val firstReply: Future[String] = mkCollection(eolCollectionJson, authHeader, urlString)
    val newCollectionId: JsValue = Json.parse(firstReply()) \ "id"
    newCollectionId should not(be(null))
    println("collection created " + newCollectionId + ".")

    println("searching for newly created collection ...")

    println("found collection.")

    val updatedCollection = Json.obj("collection" -> Json.obj("name" -> "my updated collection"
      , "description" -> "my updated description"
      , "id" -> newCollectionId
      , "collection_items" -> (
        Seq(208553L) map (preyId => {
          Json.obj(
            "collected_item_type" -> "TaxonConcept",
            "collected_item_id" -> preyId
          )
        })
        )
    ))

    val myRequest = url(urlString + """/""" + newCollectionId)
    def myPost = myRequest.PUT <:< authHeader
    def myRequestAsJson = myPost.setContentType("application/json", "UTF-8")
    def myPostWithBody = myRequestAsJson << Json.stringify(updatedCollection)
    println("updating collection [" + newCollectionId + "] ...")
    val secondReply = Http(myPostWithBody OK as.String)
    val otherCollectionId: JsValue = Json.parse(secondReply()) \ "id"
    println("updated collection [" + newCollectionId + "].")
    otherCollectionId should be(newCollectionId)


    println("deleting collection [" + newCollectionId + "] ...")
    val reply = Http(url("http://bocce.eol.org/wapi/collections/" + newCollectionId).DELETE OK as.String)
    reply()

    //    val thirdReply: Future[String] = mkCollection(eolCollectionJson, headers, urlString + """/""" + newCollectionId)
    //    val yetAnotherCollectionId: JsValue = Json.parse(thirdReply()) \ "id"
    //    yetAnotherCollectionId should not(be(newCollectionId))
    println("collection [" + newCollectionId + "] deleted.")
  }

  def mkCollection(eolCollectionJson: JsObject, headers: Map[String, String], urlString: String): Future[String] = {
    val myRequest = url(urlString)
    def myPost = myRequest.POST
    def myPostWithParams = myPost <:< headers
    def myRequestAsJson = myPostWithParams.setContentType("application/json", "UTF-8")
    def myPostWithBody = myRequestAsJson << Json.stringify(eolCollectionJson)
    val reply = Http(myPostWithBody OK as.String)
    reply
  }
}