package de.htwg.parser.utils

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.writePretty

trait JsonObject {
  implicit val formats = DefaultFormats
  def toJSON: String = writePretty(this)
}
