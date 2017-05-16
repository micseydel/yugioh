package yugioh

import java.util
import java.util.Collections

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object Util {
  /**
    * Allows Scala syntax of
    *
    * 5 times { block }
    *
    * From http://stackoverflow.com/a/2842640/1157440
    */
  implicit def intWithTimes(n: Int): Object {} = new {
    def times(f: => Unit): Unit = 1 to n foreach { _ => f }
  }

  /**
    * Remove from a mutable buffer a particular element.
    *
    * TODO LOW: want to make this an implicit in the utils, clean up the two deck situations
    */
  def remove[T](buffer: ListBuffer[T], element: T): Unit = buffer.remove(buffer.indexOf(element))
}

object Constants {
  val InitialHandSize = 5
  val HandSizeLimit = 6
  val InitialLifePoints = 8000
}

trait RandomnnessModule {
  def shuffle(xs: ListBuffer[_]): Unit
}

trait RandomnessModuleComponent {
  def randomness: RandomnnessModule
}

trait DefaultRandomnessModuleComponent extends RandomnessModuleComponent {
  //noinspection ConvertExpressionToSAM
  def randomness: RandomnnessModule = new RandomnnessModule {
    override def shuffle(xs: ListBuffer[_]): Unit = Collections.shuffle(util.Arrays.asList(xs: _*))
  }
}
