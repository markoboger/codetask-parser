package controllers

import javax.inject._
import play.api.mvc._
import play.api.Configuration
import repository.{Github, Parser, Firebase}

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
      (for {
        filesContent <- request.body.asJson match {
          case Some(value) => {
            Some(git.push(value))
          }
          case None => None
        }
        parsedFiles <- parser.parse(filesContent)
        fire <- if (parsedFiles.map(course => firebase.updateCourse(course)).contains(None)) None else Some("")
      } yield parsedFiles) match {
        case Some(fc) => Ok("")
        case None => BadRequest
      }
    } else {
      Unauthorized
    }
  }}
}
