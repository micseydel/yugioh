package yugioh

import yugioh.action.Action

import scala.collection.mutable.ListBuffer

case class MutableGameState(var turnCount: Int = 0,
                            var hasNormalSummonedThisTurn: Boolean = false,
                            var history: ListBuffer[Action] = new ListBuffer)
