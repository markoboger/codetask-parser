package controllers

import akka.util.ByteString
import javax.inject.{Inject, Singleton}
import play.api.http.HttpEntity
import play.api.mvc._
import shared.{Failure, Running, State, Success}
import models.StatusImages
import play.api.libs.ws._

import scala.collection.immutable._
import repository.Firebase

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, ws: WSClient, firebaseO: Firebase) extends AbstractController(cc) {
  def statusResult(image: Array[Byte]): Result = Result(
    header = ResponseHeader(200, Map.empty),
    body = HttpEntity.Strict(ByteString.fromArray(image), Some("image/png"))
  )
  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    State.state match {
      case Success => Ok(views.html.succeed())
      case Failure => Ok(views.html.failed())
      case Running => Ok(views.html.runnig())
    }
  }

  def status() = Action { implicit request: Request[AnyContent] =>
    State.state match {
      case Success => statusResult(StatusImages.SUCCEED)
      case Failure => statusResult(StatusImages.FAILED)
      case Running => statusResult(StatusImages.RUNNING)
    }
  }

  def firebase() = Action { implicit request: Request[AnyContent] => {
    import play.api.libs.json._
    import scala.concurrent.duration._
    import models.tables.Course

    //println(firebaseO.getCourses)
    //println(firebaseO.getUsers)
    //println(firebaseO.getUser("-L6i5HMG60EiVuOntjXK"))
    //println(firebaseO.getCourse("-L6Nj4WIYnQprOe8dY9c"))
    //firebaseO.setUserData("-L6i5HMG60EiVuOntjXK", Json.obj("autoNext" -> true), List("settings"))
    //firebaseO.setCourseData("-L6Nj4WIYnQprOe8dY9c", Json.obj("id" -> 1))
    /*firebaseO.createCourse(Course(
      2,
      "Foo",
      "Foo",
      Json.arr()
    ).toJson)*/

    println(JsString("").isInstanceOf[JsValue])

    Ok("firebase")
  }}
}
