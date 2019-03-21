package repository

import java.util.NoSuchElementException
import java.util.concurrent.TimeoutException

import play.api.libs.json._
import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.libs.ws.{WSClient, WSRequest}

import scala.concurrent.Await
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

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  @throws(classOf[NoSuchElementException])
  private def getAllCoursesRequest: Map[String, Course] = {
    val request: WSRequest = this.createRequest(config.get[String]("firebase.tables.courses"))
    val response = Await.result(request.get(), config.get[Int]("firebase.timeout").millis)
    if (response.status != 200) {
      throw new IllegalAccessException(s"Could not get all users! (Status code is not 200)")
    }
    val valuesMap: scala.collection.mutable.Map[String, Course] = new scala.collection.mutable.HashMap[String, Course]
    if (response.body.equals("null")) {
      Map.empty
    } else {
      try {
        response.json.as[JsObject].keys.foreach(key => {
          valuesMap += (key -> (response.json \ key).get)
        })
      } catch {
        case _: NoSuchElementException =>
          throw new NoSuchElementException("Could not get all courses! (Could not parse the answer!)")
      }
      valuesMap.toMap
    }
  }


  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  @throws(classOf[NoSuchElementException])
  def getCourses: Map[String, Course] = {
    if (this.courses.isEmpty) {
      this.courses = this.getAllCoursesRequest
    }
    this.courses
  }


  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  @throws(classOf[NoSuchElementException])
  def getCourse(id: String) = {
    if (this.courses.isEmpty) {
      this.courses = this.getAllCoursesRequest
    }
    this.courses(id)
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  @throws(classOf[NoSuchElementException])
  def updateCourse(course: Course): Unit = {
    this.getCourses.find{case (id, currentCourse) => currentCourse.id == course.id} match {
      case Some((id, _)) => this.setCourseData(id, course)
      case None => {
        this.createCourse(course)
        this.courses = this.getAllCoursesRequest
      }
    }
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  def createCourse(data:JsValue): Unit = {
    val request = this.createRequest(config.get[String]("firebase.tables.courses")).post(data)
    if (Await.result(request, config.get[Int]("firebase.timeout").millis).status != 200) {
      throw new IllegalAccessException(s"Course could not be created! (Status code is not 200)")
    }
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  def setCourseData(id: String, data: JsValue, attribute: List[String] = List.empty): Unit = {
    val request = this.createRequest(config.get[String]("firebase.tables.courses"), List(id) ++ attribute).patch(data)
    if (Await.result(request, config.get[Int]("firebase.timeout").millis).status != 200) {
      throw new IllegalAccessException(s"Course (${id}) could not be set! (Status code is not 200)")
    }
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  @throws(classOf[NoSuchElementException])
  def getUsers: Map[String, User] = {
    val request: WSRequest = this.createRequest(config.get[String]("firebase.tables.users"))
    val response = Await.result(request.get(), config.get[Int]("firebase.timeout").millis)
    if (response.status != 200) {
      throw new IllegalAccessException(s"Could not get all users! (Status code is not 200)")
    }
    val valuesMap: scala.collection.mutable.Map[String, User] = new scala.collection.mutable.HashMap[String, User]
    try {
      response.json.as[JsObject].keys.foreach(key => {
        valuesMap += (key -> (response.json \ key).get)
      })
    } catch {
      case _: NoSuchElementException =>
        throw new NoSuchElementException("Could not get all users! (Could not parse the answer!)")
    }
    valuesMap.toMap
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  def getUser(id: String): User = {
    val response = this.createRequest(config.get[String]("firebase.tables.users"), List(id))
      .get()
    val result = Await.result(response, config.get[Int]("firebase.timeout").millis)
    if (result.status != 200) {
      throw new IllegalAccessException(s"Could not get user (${id})! (Status code is not 200)")
    }
    result.json
  }

  @throws(classOf[TimeoutException])
  @throws(classOf[IllegalAccessException])
  def setUserData(id: String, data: JsValue, attribute: List[String] = List.empty): Unit = {
    val request = this.createRequest(config.get[String]("firebase.tables.users"), List(id) ++ attribute).patch(data)
    if (Await.result(request, config.get[Int]("firebase.timeout").millis).status != 200) {
      throw new IllegalAccessException(s"User (${id}) could not be set! (Status code is not 200)")
    }
  }
}
