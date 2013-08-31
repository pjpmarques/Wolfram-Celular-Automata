package pmarques

import processing.core._
import org.apache.commons.cli._

/**
 * Represents a Wolfram 1D celular automata calculation interface.
 */
trait WolframFactory {

  /** A board is simply a two dim array (NxM) */
  type Board = Array[Array[Char]]

  /**
   * Returns a grid containing a one dimensional celular automata according to Wolfram's rules.
   * See http://mathworld.wolfram.com/Rule30.html
   *
   * @param rule The wolfram rule (0 <= rule <= 255)
   * @param generations Number of generations (>0) for which the automata runs.
   * @return a 2D grid containing the simulation. The cell is either '0' or '1' and the grid size will be generations X generations.
   */
  def createBoard(rule: Int, generations: Int): Board
}

/**
 * Concrete implementation of a celular automata generator.
 * Only one method is provided which returns an instance of a board for a number of generations.
 */
object MyWolfram extends WolframFactory {
  def createBoard(rule: Int, generations: Int): Board = {
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

  lazy val board: WolframFactory#Board = makeBoard(0, Nil).toArray

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
  private var generations: Int = _
  private var side: Int = _
  private var rule: Int = _
  private var ruleBoard: WolframFactory#Board = _

  //-----------------------------------------------------------------

  override def setup(): Unit = {
    rule = args(0).toInt
    generations = args(1).toInt
    side = args(2).toInt
    ruleBoard = MyWolfram.createBoard(rule, generations)

    val maxX = (2*generations - 1)*side
    val maxY = generations*side

    size(maxX, maxY)
    background(255)
    noStroke()
    noLoop()
  }

  override def draw(): Unit = {
    for (y <- 0 until generations; x <- 0 until (2*generations-1)) {
      ruleBoard(y)(x) match {
        case '0' => fill(255, 255, 255)
        case '1' => fill(0, 0, 0)
      }
      rect(x*side, y*side, side, side)
    }
  }
}

object Wolfram {

  /** Class for representing the command line options of the program.
    *
    * @param rule The rule to evaluate.
    * @param generations Number of generations to run.
    * @param side Size of each square in the plot
    */
  case class WolframOptions(rule: String = "30", generations: String = "100", side: String = "5")

  /**
   * Parse the command line arguments
   * @param args The commmand line
   * @return The arguments already parsed
   */
  def parseCommandLine(args: Array[String]): WolframOptions = {
    val prgOptions = new Options
    prgOptions.addOption("help", false, "help, prints this message")
    prgOptions.addOption("rule", true, "rule number to use (default: 30)")
    prgOptions.addOption("gen", true, "generations to run (default: 50)")
    prgOptions.addOption("side", true, "side of each square in drawing (default: 5)")

    val cmdOptions: CommandLine = try {
      (new GnuParser).parse(prgOptions, args)
    }
    catch {
      case optionException: UnrecognizedOptionException => {
        System.err.println(optionException.getMessage)
        (new HelpFormatter).printHelp("Wolfram", prgOptions, true)
        sys.exit(0)
      }
    }

    if (cmdOptions.getArgs.length != 0) {
      System.err.println("Invalid option specified.")
      (new HelpFormatter).printHelp("Wolfram", prgOptions, true)
      sys.exit(0)
    }

    if (cmdOptions hasOption "help") {
      (new HelpFormatter).printHelp("Wolfram", prgOptions, true)
      sys.exit(0)
    }

    // Parse all the options into an object by using foldLeft and copying the options one by one
    (WolframOptions() /: cmdOptions.getOptions) { (tstOptions: WolframOptions, current: Option) =>
      current.getOpt match {
        case "rule"     => tstOptions.copy(rule = current.getValue)
        case "gen"      => tstOptions.copy(generations = current.getValue)
        case "side"     => tstOptions.copy(side = current.getValue)
      }
    }
  }

  /**
   * Program's main entry point
   * @param args Command line arguments
   */
  def main(args: Array[String]): Unit = {
    val options = parseCommandLine(args)
    PApplet.main(Array("pmarques.WolframApplet", options.rule, options.generations, options.side))
  }
}
