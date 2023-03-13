package com.compilers.demo.parsing

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestContextFreeGrammar extends AnyFunSuite {
  test("testing CFGrammar") {
    testGrammarProduction()
  }

  private def testGrammarProduction(): Unit =
    val rule1 = ContextFreeProductionRule("S -> ac   A B d")
    val rule2 = ContextFreeProductionRule("A ->")
    val rule3 = ContextFreeProductionRule("B -> e")
    val cfg = ContextFreeGrammar("S", List(rule1, rule2, rule3))

    println(cfg)
}
