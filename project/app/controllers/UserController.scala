package controllers

import javax.inject._
import play.api.mvc._
import repository.{Github, Parser, Firebase}


class UserController @Inject()(
    cc: ControllerComponents,
    firebase: Firebase
  ) extends AbstractController(cc) {
  def index() = Action { implicit request: Request[AnyContent] => {
    val users = firebase.getUsers

  }}
}