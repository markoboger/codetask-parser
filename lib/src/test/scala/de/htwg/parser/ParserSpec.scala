package de.htwg.parser

import org.scalatest.WordSpec

import scala.util.parsing.input.{CharArrayReader, Reader}

class ParserSpec extends WordSpec {
  val parser = new Parser
  implicit def textToInput(text: String): Reader[Char] =
    new CharArrayReader(text.toCharArray, 0).asInstanceOf[Reader[Char]]

  // region Line tests
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
  // endregion

  // region Quote tests
  "A quoted string" should {
    "return a string with single quotes escaped" in {
      val parsed = parser.quotedString.apply("\"value\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "\"value\"")
    }
    "return a string with triple quotes escaped" in {
      val parsed = parser.quotedString.apply("\"\"\"value\"\"\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "\"\"\"value\"\"\"")
    }
    "return a string with no quotes instead of single quotes" in {
      val parsed = parser.string.apply("\"value\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value")
    }
    "return a string with no quotes instead of triple quotes" in {
      val parsed = parser.string.apply("\"\"\"value\"\"\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value")
    }
  }

  "A quoted string with an escaped quote" should {
    "return a string with single quotes escaped and double escape the escaped quote" in {
      val parsed = parser.quotedString.apply("\"value\\\"\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "\"value\\\"\"")
    }
    "return a string with triple quotes escaped and an fourth quote" in {
      val parsed = parser.quotedString.apply("\"\"\"value\"\"\"\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "\"\"\"value\"\"\"\"")
    }
    "return a string with no quotes and escape the escaped quote" in {
      val parsed = parser.string.apply("\"value\\\"\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value\"")
    }
    "return a string with no quotes and a quote" in {
      val parsed = parser.string.apply("\"\"\"value\"\"\"\"")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "value\"")
    }
  }

  "A wrong amount of qoutes" should {
    "result in an error" in {
      val parsed = parser.quotedString.apply("\"value")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg == "String is not closed")
    }
    "result in a same error" in {
      val parsed = parser.quotedString.apply("\"\"\"value\"\"")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg == "String is not closed")
    }
    "result in a same error as the first test case" in {
      val parsed = parser.string.apply("\"value")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg == "String is not closed")
    }
    "result in a same error as the second test case" in {
      val parsed = parser.string.apply("\"\"\"value\"\"")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg == "String is not closed")
    }
  }
  // endregion

  "A comment line" should {
    "contain the comment line" in {
      val parsed = parser.comment.apply("//value")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "//value")
    }
    "return an error" in {
      val parsed = parser.comment.apply("/value")
      assert(!parsed.successful)
    }
  }

  "A curly bracket" should {
    "be parsed" in {
      val parsed = parser.curBrackets.apply(
        s"""{
          |// comment
          |line
          |{}
          |"string"
          |${"\""}""string""${"\""}
          |()
          |}""".stripMargin)
      assert(parsed.successful)
      assert(parsed.getOrElse("") == s"""{
                                       |// comment
                                       |line
                                       |{}
                                       |"string"
                                       |${"\""}""string""${"\""}
                                       |()
                                       |}""".stripMargin)
    }
    "parse only the first closing bracket" in {
      val parsed = parser.curBrackets.apply(
        """{
          |}
          |}""".stripMargin)
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "{\n}")
    }
    "not be parsed, because there is no closing bracket" in {
      val parsed = parser.curBrackets.apply(
        """{
          |{
          |}""".stripMargin)
      assert(!parsed.successful)
    }
  }

  "A bracket" should {
    "be parsed" in {
      val parsed = parser.brackets.apply(
        s"""(
          |// comment
          |line
          |{}
          |"string"
          |${"\""}""string""${"\""}
          |()
          |)""".stripMargin)
      assert(parsed.successful)
      assert(parsed.getOrElse("") == s"""(
                                       |// comment
                                       |line
                                       |{}
                                       |"string"
                                       |${"\""}""string""${"\""}
                                       |()
                                       |)""".stripMargin)
    }
    "parse only the first closing bracket" in {
      val parsed = parser.brackets.apply(
        """(
          |)
          |)""".stripMargin)
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "(\n)")
    }
    "not be parsed, because there is no closing bracket" in {
      val parsed = parser.brackets.apply(
        """(
          |(
          |)""".stripMargin)
      assert(!parsed.successful)
    }
  }

  "A description" should {
    "be parsed" in {
      val parsed = parser.description.apply("(\"description\")")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "description")
    }
    "be parsed as combined" in {
      val parsed = parser.description.apply("(\"description\" + \" additional\")")
      assert(parsed.successful)
      assert(parsed.getOrElse("") == "description additional")
    }
    "not be parsed without an opening bracket" in {
      val parsed = parser.description.apply("\"description\")")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg.contains("\\s*\\(\\s*"))
    }
    "not be parsed without a closing bracket" in {
      val parsed = parser.description.apply("(\"description\"")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg.contains("\\s*\\)"))
    }
    "not be parsed without a following string, if a + is present" in {
      val parsed = parser.description.apply("(\"description\" + )")
      assert(!parsed.successful)
      assert(parsed.asInstanceOf[parser.NoSuccess].msg == "'\"' expected but ')' found")
    }
  }
}
