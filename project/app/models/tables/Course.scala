package models.tables

import play.api.libs.json._

case class Course(id: Long, title: String, description: String, chapters: JsArray) {
  def toJson: JsObject = Json.obj(
    "id" -> id,
    "title" -> title,
    "description" -> description,
    "chapters" -> chapters
  )
}

object Course {
  implicit def fromJson(jsObject: JsValue): Course = Course(
    (jsObject \ "id").as[Long],
    (jsObject \ "title").as[String],
    (jsObject \ "description").as[String],
    (jsObject \ "chapters").as[JsArray]
  )

  implicit def courseToJson(course: Course): JsValue = course.toJson
}
