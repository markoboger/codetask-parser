package de.htwg.parser

import org.scalatest.WordSpec

import scala.util.parsing.input.{CharArrayReader, Reader}

class ParserSpec extends WordSpec {
  val parser = new Parser
  implicit def textToInput(text: String): Reader[Char] =
    new CharArrayReader(text.toCharArray, 0).asInstanceOf[Reader[Char]]

  "An empty line" should {
    "be empty" in {
      val parsed = parser._line.apply("")
      assert(!parsed.successful)
    }
  }

  "A value line" should {
    "contain the value" in {
      val parsed = parser._line.apply("value")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value")
    }
    "contain the value with a break" in {
      val parsed = parser._line.apply("value\n")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value\n")
    }
  }

  "A value without until line" should {
    "contain only the value" in {
      val parsed = parser._line("until".r).apply("value\n")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value\n")
    }
  }

  "A value with an until line" should {
    "contain only the value" in {
      val parsed = parser._line("until".r).apply("value until\n")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value ")
    }
  }

  "An until line" should {
    "cause an error" in {
      val parsed = parser._line("until".r).apply("until value\n")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg == "Reached the expected line: until")
    }
  }
}
