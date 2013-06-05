package pmarques

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WolframTest extends FunSuite {

  test("Rule 30 test - empty") {
    intercept[IllegalArgumentException] {
  	  val rule30_empty = MyWolfram.createBoard(30, 0)
    }
  }

  test("Illegal rules (<0 and >255)") {
    intercept[IllegalArgumentException] {
      val rule_minus1 = MyWolfram.createBoard(-1, 5)
    }

    intercept[IllegalArgumentException] {
      val rule_above255 = MyWolfram.createBoard(256, 5)
    }
  }

  test("Rule 30 test - one generation") {
    val rule30_oneLevel = MyWolfram.createBoard(30, 1)
    assert(rule30_oneLevel === Array(Array('1')))
  }

  test("Rule 30 test - two generations") {
    val rule30_twoLevels = MyWolfram.createBoard(30, 2)
    assert(rule30_twoLevels === Array(
      Array('0', '1', '0'),
      Array('1', '1', '1')))
  }

  test("Rule 30 test - five generations") {
    val rule30_result = MyWolfram.createBoard(30, 5) map (_.mkString) mkString ("\n")
    val rule30_fiveLevelsExpected = 
      """|000010000
         |000111000
         |001100100
         |011011110
         |110010001""".stripMargin
	                                   		      
	  assert(rule30_result === rule30_fiveLevelsExpected)
  }

  test("Rule 222 test (sanity check) - five generations") {
    val rule222_result = MyWolfram.createBoard(222, 5) map (_.mkString) mkString ("\n")
    val rule222_fiveLevelsExpected =
      """|000010000
         |000111000
         |001111100
         |011111110
         |111111111""".stripMargin

    assert(rule222_result === rule222_fiveLevelsExpected)
  }

  test("Rule 132 test (sanity check) - five generations") {
    val rule132_result = MyWolfram.createBoard(132, 5) map (_.mkString) mkString ("\n")
    val rule132_fiveLevelsExpected =
      """|000010000
         |000010000
         |000010000
         |000010000
         |000010000""".stripMargin

    assert(rule132_result === rule132_fiveLevelsExpected)
  }
}
