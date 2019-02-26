package models.tables

import play.api.libs.json._

case class User(uid: String, displayName: String, email: String, role: String, score: Int, settings: UserSettings) {
  def toJson: JsObject = Json.obj(
    "uid" -> uid,
    "displayName" -> displayName,
    "email" -> email,
    "role" -> role,
    "score" -> score,
    "settings" -> settings.toJson
  )
}

object User {
  implicit def fromJson(jsObject: JsValue): User = User(
    (jsObject \ "uid").as[String],
    (jsObject \ "displayName").as[String],
    (jsObject \ "email").as[String],
    (jsObject \ "role").as[String],
    (jsObject \ "score").as[Int],
    (jsObject \ "settings").as[JsValue]
  )
}

case class UserSettings(autoNext: Boolean, autoPlay: Boolean) {
  def toJson: JsObject = Json.obj(
    "autoNext" -> autoNext,
    "autoPlay" -> autoPlay
  )
}

object UserSettings {
  implicit def fromJson(jsObject: JsValue): UserSettings = UserSettings(
    (jsObject \ "autoNext").as[Boolean],
    (jsObject \ "autoPlay").as[Boolean]
  )
  implicit def fromJson(jsObject: JsObject): UserSettings = UserSettings(
    (jsObject \ "autoNext").as[Boolean],
    (jsObject \ "autoPlay").as[Boolean]
  )
}