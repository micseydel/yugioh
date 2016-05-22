package yugioh

object Util {
  /**
    * Allows Scala syntax of
    *
    * 5 times { block }
    *
    * From http://stackoverflow.com/a/2842640/1157440
    */
  implicit def intWithTimes(n: Int): Object {def times(f: => Unit): Unit} = new {
    def times(f: => Unit) = 1 to n foreach { _ => f }
  }
}

object Constants {
  val InitialHandSize = 5
  val HandSizeLimit = 6
  val InitialLifePoints = 8000
}
