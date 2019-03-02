package controllers

import javax.inject._
import models.tables.Course
import play.api.mvc._
import play.api.Configuration
import repository.{Firebase, Github, Parser}
import shared.State

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class ParserController @Inject()(
    cc: ControllerComponents,
    git: Github,
    parser: Parser,
    firebase: Firebase,
    config: Configuration
  ) extends AbstractController(cc) {
  def github(key: String) = Action { implicit request: Request[AnyContent] => {
    if (key == config.get[String]("github.key")) {
      try {
        State.running()
        val parsed: Option[List[Course]] = for {
          body <- request.body.asJson
          filesContent <- Some(git.push(body))
          parsedFiles <- Some(parser.parse(filesContent))
        } yield parsedFiles

        parsed match {
          case Some(p) => p.foreach(course => firebase.updateCourse(course))
          case None => throw new NoSuchElementException("Request body could not be parsed!")
        }
        State.succeed()
        Ok("")
      } catch {
        case err: Exception => {
          State.failed(err)
          BadRequest
        }
      }
    } else {
      Unauthorized
    }
  }}
}
