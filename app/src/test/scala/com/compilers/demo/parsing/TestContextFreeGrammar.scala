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
    val rule1 = ContextFreeProductionRule("S -> ac tt M  K     A B d")
    val rule2 = ContextFreeProductionRule("A ->")
    val rule3 = ContextFreeProductionRule("B -> e")
    val rule4 = ContextFreeProductionRule("K -> epsilon")
    val rule5 = ContextFreeProductionRule("M -> ε")
    val cfg = ContextFreeGrammar("S", List(rule1, rule2, rule3, rule4, rule5))

    assert(cfg.nonTerminals.size == 5) // S M K A B
    assert(cfg.terminals.size == 5) // ac tt d e ε
    assert(cfg.symbols.size == 10)

    println(cfg)
}
