package repository

import play.api.libs.json._
import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.libs.ws.{WSClient, WSRequest}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import models.tables._

@Singleton
class Firebase @Inject() (config: Configuration, ws: WSClient) {
  private var courses: Map[String, Course] = Map.empty

  private def createRequest(table: String, additional: List[String] = List.empty): WSRequest =
    ws.url(s"${config.get[String]("firebase.database")}${table}/${additional.mkString("/")}.json")
    .addHttpHeaders("Accept" -> "application/json")
    .withRequestTimeout(config.get[Int]("firebase.timeout").millis)

  private def getAllCoursesRequest: Map[String, Course] = {
    val request: WSRequest = this.createRequest(config.get[String]("firebase.tables.courses"))
    val response: Future[Map[String, Course]] = request.get().map { values => {
      val valuesMap: scala.collection.mutable.Map[String, Course] = new scala.collection.mutable.HashMap[String, Course]
      values.json.as[JsObject].keys.foreach(key => {
        valuesMap += (key -> (values.json \ key).get)
      })
      valuesMap.toMap
    }}
    Await.result(response, Duration.Inf)
  }

  def getCourses: Map[String, Course] = {
    if (this.courses.isEmpty) {
      this.courses = this.getAllCoursesRequest
    }
    this.courses
  }

  def getCourse(id: String) = {
    if (this.courses.isEmpty) {
      this.courses = this.getAllCoursesRequest
    }
    this.courses(id)
  }

  def updateCourse(course: Course): Unit = {
    this.getCourses.find{case (id, currentCourse) => currentCourse.id == course.id} match {
      case Some((id, _)) => this.setCourseData(id, course)
      case None => {
        this.createCourse(course)
        this.courses = this.getAllCoursesRequest
      }
    }
    Some("")
  }

  def createCourse(data:JsValue): Unit = {
    val request = this.createRequest(config.get[String]("firebase.tables.courses")).post(data)
    Await.result(request, Duration.Inf).status match {
      case 200 => ()
      case _ => ()
    }
  }

  def setCourseData(id: String, data: JsValue, attribute: List[String] = List.empty): Unit = {
    val request = this.createRequest(config.get[String]("firebase.tables.courses"), List(id) ++ attribute).patch(data)
    Await.result(request, Duration.Inf).status match {
      case 200 => ()
      case _ => ()
    }
  }

  def getUsers: Map[String, User] = {
    val request: WSRequest = this.createRequest(config.get[String]("firebase.tables.users"))
    val response: Future[Map[String, User]] = request.get().map { values => {
      val valuesMap: scala.collection.mutable.Map[String, User] = new scala.collection.mutable.HashMap[String, User]
      values.json.as[JsObject].keys.foreach(key => {
        valuesMap += (key -> (values.json \ key).get)
      })
      valuesMap.toMap
    }}
    Await.result(response, Duration.Inf)
  }

  def getUser(id: String): User = {
    val response: Future[User] = this.createRequest(config.get[String]("firebase.tables.users"), List(id))
      .get()
      .map {_.json}
    Await.result(response, Duration.Inf)
  }

  def setUserData(id: String, data: JsValue, attribute: List[String] = List.empty): Boolean = {
    val request = this.createRequest(config.get[String]("firebase.tables.users"), List(id) ++ attribute).patch(data)
    Await.result(request, Duration.Inf).status match {
      case 200 => true
      case _ => false
    }
  }
}
