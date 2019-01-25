package de.htwg.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader

import de.htwg.parser.utils._
import de.htwg.parser.error.ParsingError


class Parser extends RegexParsers {
  override val skipWhitespace = false
  implicit def textToInput(text: String): Input = new CharArrayReader(text.toCharArray, 0).asInstanceOf[Input]

  private[parser] def _EOF: Regex = "\\z".r
  private[parser] def _LINE_BREAK = "(\\n|\\r\\n)".r

  private[parser] def _line: Parser[String] = this._line(_EOF)
  private[parser] def _lineUntilAppearance(until: Regex): Parser[String] = s"[^\\n\\r]+?(?=${until.regex})".r ^^
    { _.replaceAll("^\\s+", "") }
  private[parser] def _line(until: Regex): Parser[String] = not(this._EOF) ~> (
    not(until) ~> guard(s"[^\\n\\r]+${until.regex}".r) ~> _lineUntilAppearance(until) |
      not(until) ~> "[^\\n\\r]*".r ~ opt("[\\n\\r]".r) ^^ {
        case l ~ Some(_) => l.trim + "\n"
        case l ~ None => l.trim
      } |
      failure(s"Reached the expected line: ${until.regex}"))

  // region String parsing
  private[parser] def quotedString: Parser[String] = "\"\"\"" ~>
    commit((("[\\s\\S]*?(?=\"\"\"([^\"]|\\z))".r withFailureMessage stringNotClosed) ^^
      { v => s"""${"\""}""$v""${"\""}""" }) <~ "\"\"\"") | "\"" ~>
    commit((("[\\s\\S]*?((?<!\\\\)|(?<!\\\\)(\\\\\\\\)+)(?=\")".r withFailureMessage stringNotClosed) ^^
      { v => s""""$v"""" }) <~ "\"")
  private[parser] def stringNotClosed = "String is not closed"
  private[parser] def stringThreeQuotes: Parser[String] = "\"\"\"" ~> ("[\\s\\S]*?(?=\"\"\"([^\"]|\\z))".r withFailureMessage stringNotClosed) <~
    "\"\"\""
  private[parser] def stringOneQuote: Parser[String] = "\"" ~>
    ("[^\\r\\n]*?((?<!\\\\)|(?<!\\\\)(\\\\\\\\)+)(?=\")".r withFailureMessage stringNotClosed) <~ "\"" ^^
    { _.replaceAll("\\\\\"", "\"") }
  private[parser] def string: Parser[String] = guard("\"\"\"") ~> commit(stringThreeQuotes) | guard("\"") ~> commit(stringOneQuote)
  // endregion

  // // comment
  private[parser] def comment: Parser[String] = "[ \\t]*\\/\\/[^\\r\\n]*".r

  // region Brackets ()/{}
  def curBracketsContent: Parser[String] =
    rep(comment | _line("[\\{\\(\"\\}]".r) | curBrackets | brackets | quotedString) ^^ ( _.mkString )
  def curBrackets: Parser[String] = "{" ~> curBracketsContent <~ "}" ^^ { v => s"{${v}}" }

  def bracketsContent: Parser[String] =
    rep(comment | _line("[\\{\\(\"\\)]".r) | curBrackets | brackets | quotedString) ^^ ( _.mkString )
  def brackets: Parser[String] = "(" ~> bracketsContent <~ ")" ^^ { v => s"(${v})" }
  // endregion

  // description combining strings with a + sign
  private[parser] def description: Parser[String] = "\\s*\\(\\s*".r ~> string ~ opt(rep1("\\s*\\+\\s*".r ~> commit(string))) <~ "\\s*\\)".r ^^ {
    case first ~ Some(text) => first + text.mkString
    case text ~ None => text
  }

  //region Koan
  private[parser] def assert: Parser[(String, String)] = "(" ~>
    (assertBool | (
      rep1(_line("(\"|\\s*==|\\s*===|\\seq\\s|\\s*\\.\\s*eq)".r) | quotedString) ~ (
        "\\s*===\\s*".r ~> compare(3) |
          "\\s*==\\s*".r ~> compare() |
          "\\seq\\s*".r ~> gapEqual |
          "\\s*\\.\\s*eq".r ~> equal
        ) ^^ { case before ~ after => (s"${before.mkString}${after._1}", after._2)}
      ))
  private[parser] def assertBool: Parser[(String, String)] = ("\\s*true\\s*".r | "\\s*false\\s*".r) ^^ { v => ("__", v.trim) }
  private[parser] def compare(signs: Int = 2): Parser[(String, String)] = "\\s*".r ~> rep1(_line("(\"|\\)|,)".r) | quotedString) ^^
    { v => (s" ${"=" * signs} __", v.mkString.trim) }
  private[parser] def gapEqual: Parser[(String, String)] = rep1(_line("(\"|\\)|,)".r) | quotedString) ^^
    { v => (" eq __", v.mkString.trim) }
  private[parser] def equal: Parser[(String, String)] = "(" ~> bracketsContent <~ ")" ^^
    { v => (".eq(__)", v.mkString)}
  private[parser] def assertString(value: Any): (String, String) = value match {
    case (internal, answer: String) ~ Some(text) => (s"assert($internal, $text)", answer)
    case (internal, answer: String) ~ None => (s"assert($internal)", answer)
  }

  private[parser] def shouldBe: Parser[(String, String)] = "(" ~> bracketsContent <~ ")" ^^ { v => ("should be(__)", v) }
  private[parser] def koanTasks: Parser[(String, List[String])] = rep1(
    "assert\\s*".r ~> commit(assert) ~ opt("\\s*,\\s*".r ~> quotedString) <~ ")" ^^ { assertString } |
      _line("(\\{|\\(|\"|\\}|should\\s+be|assert)".r) |
      "should\\s+be\\s*".r ~> commit(shouldBe) |
      curBrackets | brackets | quotedString
  ) ^^ { body => {
    val stringBuilder = new StringBuilder()
    val answers = scala.collection.mutable.ArrayBuffer.empty[String]
    body.foreach {
      case (should: String, answer: String) => {
        stringBuilder.append(should)
        answers += answer
      }
      case line: String => stringBuilder.append(line)
    }
    (stringBuilder.toString, answers.toList)
  }}
  private[parser] def koanBody: Parser[(String, List[String])] = "\\s*\\{".r ~> koanTasks <~ "}"
  private[parser] def koan: Parser[Koan] = description ~ koanBody ^^ {
    case desc ~ body => Koan(KoanData(desc, body._1, body._2))
  }
  //endregion

  //region Video
  private[parser] def videoBody: Parser[(String, String)] = (string <~ "\\s*,\\s*".r) ~ string ^^ { case desc ~ url => (desc, url) }
  def video: Parser[Video] = "\\s*\\(\\s*".r ~> videoBody <~ "\\s*\\)".r ^^ {v => Video(VideoData(v._1, v._2))}
  //endregion

  //region Codetask
  private[parser] def codetaskSolve: Parser[List[String]] = rep(_line("\\/\\/endsolve".r)) <~ "\\/\\/endsolve\\s*".r
  private[parser] def codetaskTest: Parser[List[String]] = rep(_line("\\/\\/endtest".r)) <~ "\\/\\/endtest\\s*".r

  private[parser] def codetaskBracketsContent: Parser[String] = rep(comment | _line("[\\{\\(\"\\)]".r) | codetaskCurBracket | codetaskBrackets | quotedString) ^^ ( _.mkString )
  private[parser] def codetaskBrackets: Parser[String] = "(" ~> codetaskBracketsContent <~ ")" ^^ { v => s"(${v})" }
  private[parser] def codetaskCurBracketsContent: Parser[String] = rep(
    "\\s*\\/\\/solve".r ~> commit(codetaskSolve) ^^^ { "//todo\\n" } |
      "\\s*\\/\\/test".r ~> commit(codetaskTest) ^^^ { "" } |
      comment | _line("(\\{|\\(|\"|\\})".r) | codetaskCurBracket | codetaskBrackets | quotedString
  ) ^^ ( _.mkString )
  private[parser] def codetaskCurBracket: Parser[String] = "{" ~> codetaskCurBracketsContent <~ "}" ^^ { v => s"{${v}}" }

  private[parser] def codetaskTasks: Parser[String] = rep(
    "\\s*\\/\\/solve".r ~> commit(codetaskSolve) ^^^ { "//todo\\n" } |
      "\\s*\\/\\/test".r ~> commit(codetaskTest) ^^^ { "" } |
      _line("(\\{|\\(|\"|\\})".r) | codetaskCurBracket | brackets | quotedString
  ) ^^ { _.mkString }
  private[parser] def codetaskBody: Parser[String] = "\\s*\\{".r ~> codetaskTasks <~ "}"
  private[parser] def codetask: Parser[CodeTask] = description ~ codetaskBody ^^ {
    case desc ~ code => CodeTask(CodeTaskData(desc, code))
  }
  //endregion

  private[parser] def tasks: Parser[List[Task]] = "\\s*\\{".r ~> rep(
    "\\s*koan".r ~> commit(koan) |
      "\\s*video".r ~> commit(video) |
      "\\s*codetask".r ~> commit(codetask) |
      comment | _line("[\\{\\(\"\\}]".r) | curBrackets | brackets | quotedString
  ) <~ "\\s*\\}".r ^^ { _.filter(v => v.isInstanceOf[Task]).asInstanceOf[List[Task]] }
  private[parser] def className: Parser[Any] = _line("[ \\t\\(]".r) ~ opt(brackets) ~ "\\s*".r
  private[parser] def codetaskSuiteMeta: Parser[(String, Int)] = "CodeTaskSuite" ~> "\\s*\\(\\s*".r ~>
    (string <~ "\\s*,\\s*".r) ~ ("\\d+".r <~ "\\s*\\)".r) ^^ { case name ~ id => (name, id.toInt) }
  private[parser] def inheritance: Parser[(String, Int)] = rep(not("CodeTaskSuite") ~ className ~ "with\\s+".r) ~>
    codetaskSuiteMeta <~ rep("\\s*with\\s+".r ~> className)
  private[parser] def extend: Parser[(String, Int)] = "extends\\s+".r ~> inheritance
  private[parser] def codetaskSuite: Parser[CodeTaskSuite] = className ~> extend ~ commit(tasks) ^^ {
    case meta ~ tasks => {
      val (name, id) = meta
      CodeTaskSuite(name, id, tasks)
    }
  }
  private[parser] def unknownClass: Parser[Any] = className <~ opt("extends\\s+".r ~ rep(className ~ "with\\s+".r) ~ className) <~ opt(curBrackets)
  private[parser] def fileParser: Parser[Any] = rep(
    opt("\\s*case\\s+".r) ~> "\\s*class\\s+".r ~> commit(codetaskSuite | unknownClass) |
      _line
  ) ^^ { _.filter(v => v.isInstanceOf[CodeTaskSuite]) }

  def parse(file: String): List[CodeTaskSuite] = parseAll(fileParser, file) match {
    case Success(msg, _) => {
      val codeTaskSuites = msg.asInstanceOf[List[CodeTaskSuite]]
      codeTaskSuites.foreach(codeTaskSuite => {
        codeTaskSuite.tasks.filter(k => k.isInstanceOf[Koan]).zipWithIndex.foreach{case (k: Koan, i) => {k.id = i+1}}
        codeTaskSuite.tasks.filter(v => v.isInstanceOf[Video]).zipWithIndex.foreach{case (v: Video, i) => v.id = i+1}
        codeTaskSuite.tasks.filter(c => c.isInstanceOf[CodeTask]).zipWithIndex.foreach{case (c: CodeTask, i) => c.id = i+1}
      })
      codeTaskSuites
    }
    case NoSuccess(err, next) => throw ParsingError(err, next.pos)
  }
}
