package de.htwg.parser.error
import util.parsing.input.Position

class ParsingError(error: String) extends Exception(error) {

  def this(error: String, position: Position) = {
    this(s"(${position.line}, ${position.column}) - ${error}")
  }
}

object  ParsingError {
  def apply(error: String, position: Position): ParsingError = new ParsingError(error, position)
}