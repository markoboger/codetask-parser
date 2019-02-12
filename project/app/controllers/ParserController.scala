package controllers

import com.google.auth.oauth2.GoogleCredentials
import com.google.firebase._
import com.google.firebase.database._

import java.io.FileInputStream

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Failure => FFailure, Success => FSuccess}

import javax.inject._
import play.api.mvc._
import play.api.Configuration

import scala.beans.BeanProperty

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class ParserController @Inject()(config: Configuration, cc: ControllerComponents) extends AbstractController(cc) {
  def github() = Action { implicit request: Request[AnyContent] => {
    val serviceAccount = new FileInputStream(config.get[String]("firebase.credentials"))
    val credentials = GoogleCredentials.fromStream(serviceAccount)
    val options = new FirebaseOptions.Builder()
        .setDatabaseUrl(config.get[String]("firebase.database"))
        .setCredentials(credentials)
        .build()
    //FirebaseApp.initializeApp(options)
    val database = FirebaseDatabase.getInstance()
    Ok(database.getReference("users/LRDLKb8PqcROjAdOdC6").toString)
  }
  }
}

class User {
  @BeanProperty var displayName: String = _
  @BeanProperty var email: String = _
  @BeanProperty var role: String = _
  @BeanProperty var score: Int = _
  @BeanProperty var settings: Map[String, Boolean] = _
  @BeanProperty var uid: Int = _

  override def toString: String =
    s"""{
       |  displayName: $displayName
       |  email: $email
       |  role: $role
       |  score: $score
       |  settings: $settings
       |  uid: $uid
       |}""".stripMargin
}
