package controllers

import javax.inject._
import play.api.Configuration
import play.api.libs.json.{JsNumber, Json}
import play.api.mvc._
import repository.{Firebase, Github, Parser}


class UserController @Inject()(
    cc: ControllerComponents,
    firebase: Firebase,
    config: Configuration
  ) extends AbstractController(cc) {
  def index(key: String) = Action { implicit request: Request[AnyContent] => {
    if (key == config.get[String]("users.key")) {
      val users = firebase.getUsers
      Ok(views.html.users(key, users))
    } else {
      Unauthorized
    }
  }}

  def changeUsers(key: String, method: String) = Action { implicit request: Request[AnyContent] => {
    if (key == config.get[String]("users.key")) {
      request.body.asFormUrlEncoded match {
        case Some(data) => {
          val users = firebase.getUsers
          data.map { case (id, _) => id }.foreach(id => {
            method match {
              case "n" => if (users(id).score >= 0) {
                firebase.setUserData(id, Json.parse(s"""{"score": ${users(id).score * -1}}"""))
              }
              case "p" => if (users(id).score < 0) {
                firebase.setUserData(id, Json.parse(s"""{"score": ${users(id).score * -1}}"""))
              }
            }
          })
        }
        case None => ()
      }

      Redirect(controllers.routes.UserController.index(key))
    } else {
      Unauthorized
    }
  }}
}