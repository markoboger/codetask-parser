package shared
import de.htwg.parser.error.ParsingError

sealed trait State

case object Success extends State
case object Failure extends State
case object Running extends State

object State {
  private var _state: State = Success
  private var _error: Option[Exception] = None
  private var _file: Option[String] = None

  def state: State = this._state

  def error: Option[Exception] = this._error

  def currentFile(file: String) = this._file = Some(file)
  def getCurrentFile: Option[String] = this._file

  def failed(error: Exception): Unit = {
    this._error = Some(error)
    this._state = Failure
  }

  def succeed(): Unit = {
    this._error = None
    this._file = None
    this._state = Success
  }

  def running(): Unit = {
    this._error = None
    this._file = None
    this._state = Running
  }
}
