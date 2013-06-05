package pmarques

import processing.core._

/**
 * Represents a Wolfram 1D celular automata calculation interface.
 */
trait Wolfram {
  /**
   * Returns a grid containing a one dimensional celular automata according to Wolfram's rules.
   * See http://mathworld.wolfram.com/Rule30.html
   *
   * @param rule The wolfram rule (0 <= rule <= 255)
   * @param generations Number of generations (>0) for which the automata runs.
   * @return a 2D grid containing the simulation. The cell is either '0' or '1' and the grid size will be generations X generations.
   */
  def createBoard(rule: Int, generations: Int): Array[Array[Char]]
}

/**
 * Concrete implementation of a celular automata generator.
 * Only one method is provided which returns an instance of a board for a number of generations.
 */
object MyWolfram extends Wolfram {
  def createBoard(rule: Int, generations: Int): Array[Array[Char]] = {
    require(rule >= 0 && rule<=255)
    require(generations > 0)

    new MyWolfram(rule, generations).board
  }
}

/**
 * Which instance of MyWolfram specifies the rule to be evaluated and for how many generations
 * it should run.
 *
 * @param rule The rule to be evaluated (according to Wolfram' definition)
 * @param generations How many generations (board lines) should the system evaluate.
 */
class MyWolfram private(val rule: Int, val generations: Int) {
  val boardLength = generations*2 - 1

  lazy val board: Array[Array[Char]] = makeBoard(0, Nil).toArray

  //-----------------------------------------------------------------

  /**
   * Given a rule in binary (passed in the class constructor) create a string representation for the rule.
   * The rule is padded with 0s to the left so that exactly eight bits are represented.
   */
  private val ruleMapping: String = {
    val ruleAsBinaryString = {
      val rawBinary = rule.toBinaryString
      ("00000000" drop rawBinary.length) + rawBinary
    }
    ruleAsBinaryString.reverse
  }

  //-----------------------------------------------------------------

  /**
   * @inheritdoc
   */
  private def makeBoard(level: Int, previousLevels: List[Array[Char]]): List[Array[Char]] = level match {
    case 0 => makeBoard(1, Range(0, boardLength).map(i => if (i!=boardLength/2) '0' else '1').toArray :: Nil)
    case i if (i==generations) => previousLevels.reverse
    case i => {
      val lastLevel = previousLevels.head
      val currentLevel = Range(0, boardLength).map(pos =>
      {
        val leftCell = if (lastLevel(left(pos)) == '1') 1 else 0
        val centerCell = if (lastLevel(pos) == '1') 1 else 0
        val rightCell = if (lastLevel(right(pos)) == '1') 1 else 0
        val ruleKey = 4*leftCell + 2*centerCell + 1*rightCell                 // The rule is a number in base 2, thus use the corresponding powers of 2

        ruleMapping(ruleKey)
      }).toArray

      makeBoard(level+1, currentLevel :: previousLevels)
    }
  }

  private def left(i: Int) = if (i==0) 0 else i-1                             // Returns the cell to the left, pad to 0
  private def right(i: Int) = if (i==boardLength-1) boardLength-1 else i+1    // Returns the cell to the right, pad to 0
}


/**
 * WolframApplet is a processing applet that allows to draw the evolution of an automata.
 */
class WolframApplet extends PApplet {
  private val rule = 30                   // The rule to simulate              -- TODO: remove this from being hardcoded
  private val generations = 128           // Number of generations to simulate -- TODO: remove this from being hardcoded
  private val side = 5                    // The size of each cell

  private val ruleBoard = MyWolfram.createBoard(rule, generations)

  //-----------------------------------------------------------------

  override def setup {
    val maxX = (2*generations - 1)*side
    val maxY = generations*side

    size(maxX, maxY)
    background(255)
    noStroke()
    noLoop()
  }

  override def draw {
    for (y <- 0 until generations; x <- 0 until (2*generations-1)) {
      ruleBoard(y)(x) match {
        case '0' => fill(255, 255, 255)
        case '1' => fill(0, 0, 0)
      }
      rect(x*side, y*side, side, side)
    }
  }
}

object Main extends App {
  PApplet.main(Array("pmarques.WolframApplet"))
}
