package yugioh


/**
  * Object to encapsulate whether a sequence of objects meets certain criteria. Overloading toString is highly recommended.
  */
trait Criteria[A] {
  /**
    * Can the player possibly meet the requirements?
    */
  def meetable(implicit gameState: GameState): Boolean // TODO: this must know the player

  /**
    * Available choices to fulfill the requirements.
    */
  def availableChoices(implicit gameState: GameState): Seq[A]

  /**
    * Verify that the subject of availableChoices which has been selected is valid.
    */
  def validSelection(choices: Seq[A])(implicit gameState: GameState): Boolean
}

// TODO: specialized convenience criteria subclasses, ideally allowing for declarative style
