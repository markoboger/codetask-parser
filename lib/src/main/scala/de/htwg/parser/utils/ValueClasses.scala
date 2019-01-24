package de.htwg.parser.utils

case class CodeTaskSuite(name: String, id: Int, tasks: List[Task]) extends JsonObject
case class Koan(data: KoanData, var id: Int = -1, tag: String = "koan-task") extends Task
case class Video(data: VideoData, var id: Int = -1, tag: String = "video-task") extends Task
case class CodeTask(data: CodeTaskData, var id: Int = -1, tag: String = "code-task") extends Task
