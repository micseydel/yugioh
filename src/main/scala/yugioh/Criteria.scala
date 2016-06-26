package yugioh


/**
  * Object to encapsulate whether a sequence of objects meets certain criteria. Overloading toString is highly recommended.
  */
trait Criteria[A] {
  def validSelection(choices: Seq[A]): Boolean
}

class CountCriteria[A](needed: Int) extends Criteria[A] {
  override def validSelection(choices: Seq[A]) = choices.size == needed
  override def toString = s"$needed needed"
}

object CountCriteria {
  def apply[A](needed: Int) = new CountCriteria[A](needed)
}
