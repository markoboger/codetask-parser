package repository

import de.htwg.parser.utils.CodeTaskSuite
import de.htwg.parser.error.ParsingError
import javax.inject.{Inject, Singleton}
import de.htwg.parser.{Parser => CTParser}
import models.tables.Course
import models.tests.{FileContent, Meta, TestFile}
import play.api.Configuration
import play.api.libs.json.{JsArray, Json}

@Singleton
class Parser @Inject() (config: Configuration) {

  @throws(classOf[IllegalStateException])
  @throws(classOf[ParsingError])
  def parse(filesContent: Map[String, List[FileContent]]): List[Course] = {
    val parser = new CTParser()
    val parsedFiles = filesContent.map { case (folder, contents) => {
      folder -> contents.map(content => content match {
        case TestFile(file) => parser.parse(file)
        case meta: Meta => meta
      })
    }}
    val courses: Map[String, Option[(Meta, List[CodeTaskSuite])]] = parsedFiles.map { case (folder, contents) => {
      folder -> (for {
        meta <- contents.find(_.isInstanceOf[Meta])
        content <- Some(contents
          .filter(_.isInstanceOf[List[CodeTaskSuite]])
          .asInstanceOf[List[List[CodeTaskSuite]]]
          .flatten
        )
      } yield (meta.asInstanceOf[Meta], content))
    }}

    courses.map {
      case (_, Some(course)) => Course(
        course._1.id,
        course._1.title,
        course._1.description,
        Json.toJson(course._2.map(file => Json.parse(file.toJSON))).as[JsArray]
      )
      case (folder, _) => throw new IllegalStateException(s"Meta or content in folder '${folder}' not found!")
    }.toList
  }
}
