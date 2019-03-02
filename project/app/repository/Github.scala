package repository

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.libs.json._
import java.util.{Base64, NoSuchElementException}
import java.util.concurrent.TimeoutException

import models.tests.{FileContent, Meta, TestFile}
import play.api.libs.ws.WSClient

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class Github @Inject() (config: Configuration, ws: WSClient) {
  @throws(classOf[TimeoutException])
  @throws(classOf[NoSuchElementException])
  def push(value: JsValue): Map[String, List[(String, FileContent)]] = {
    if ((value \ "ref").isDefined && (value \ "ref").get.as[String] == config.get[String]("github.branch")) {
      try {
        val added = value \ "head_commit" \ "added"
        val modified = value \ "head_commit" \ "modified"
        val removed = value \ "head_commit" \ "removed"
        val repositoryName = value \ "repository" \ "full_name"
        val folderFiles = this.getFolderFiles(
          repositoryName.get.as[String],
          (added.get.as[JsArray] ++ modified.get.as[JsArray] ++ removed.get.as[JsArray])
            .value
            .filter(f => f.as[String].startsWith(config.get[String]("github.test_folder")))
            .map(f => f.as[String].split("/").dropRight(1).mkString("/"))
            .distinct
            .toList
        )
        this.getFilesContent(folderFiles)
      } catch {
        case _: NoSuchElementException => throw new NoSuchElementException("File paths or repository name not found!")
      }
    }
    else {
      Map.empty
    }
  }

  @throws(classOf[TimeoutException])
  private def getFolderFiles(repositoryName: String, folders: List[String]):
  Map[String, List[Option[(JsValue, JsValue)]]] = {
    val repository = config.get[String]("github.repository").replace("{repositoryName}", repositoryName)

    folders.map(f => {
      val request = ws.url(s"${repository}${f}")
        .addHttpHeaders("Accept" -> "application/json")
        .withRequestTimeout(config.get[Int]("github.timeout").millis)

      val response: Future[JsArray] = request.get().map { _.json.as[JsArray] }
      f -> Await.result(response, config.get[Int]("github.timeout").millis).value
        .filter(content => (content \ "type").toOption match {
          case Some(t) => t.as[String] == "file"
          case None => throw new NoSuchElementException("File type not found!")
        })
        .map(content => for {
          name <- (content \ "name").toOption
          url <- (content \ "url").toOption
        } yield (name, url))
        .toList

    }).toMap
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[NoSuchElementException])
  private def getFilesContent(folderFiles: Map[String, List[Option[(JsValue, JsValue)]]]):
    Map[String, List[(String, FileContent)]] = {
    folderFiles.map{ case (folder, files) => {
      folder -> files.map {
        case Some((fileName, fileUrl)) => {
          val request = ws.url(fileUrl.as[String])
            .addHttpHeaders("Accept" -> "application/json")
            .withRequestTimeout(config.get[Int]("github.timeout").millis)
          val response: Future[JsValue] = request.get().map{ _.json }
          (Await.result(response, config.get[Int]("github.timeout").millis) \ "content").toOption match {
            case Some(content) => {
              val file = new String(Base64.getDecoder.decode(content.as[String].replaceAll("\n", "")))
              if (fileName.as[String] == config.get[String]("github.meta")) {
                val jsonFile = Json.parse(file)
                val meta = for {
                  id <- (jsonFile \ "id").toOption
                  title <- (jsonFile \ "title").toOption
                  description <- (jsonFile \ "description").toOption
                } yield (id.as[Int], title.as[String], description.as[String])
                meta match {
                  case Some((id, title, description)) => (fileName.as[String], Meta(id, title, description))
                  case None => throw new NoSuchElementException("Meta could not be parsed!")
                }
              } else {
                (fileName.as[String], TestFile(file))
              }
            }
            case None => throw new NoSuchElementException("File has no content!")
          }
        }
        case None => throw new NoSuchElementException(s"File name or url could not be parsed!")
      }
    }}
  }
}
