package de.htwg.parser.utils

case class KoanData(description: String, code: String, solutions: List[String], mode: String = "scala") extends JsonObject
case class VideoData(description: String, url: String) extends JsonObject
case class CodeTaskData(description: String, code: String, mode: String = "scala") extends JsonObject
