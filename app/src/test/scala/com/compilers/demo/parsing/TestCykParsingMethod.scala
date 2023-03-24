package com.compilers.demo.parsing

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestCykParsingMethod extends AnyFunSuite {

  test("testing Chomsky Normal Form checking") {
    val startTime = System.currentTimeMillis()
    //    testChomsky()
    testChomsky2()
    val endTime = System.currentTimeMillis()
    println(s"time used: ${endTime - startTime} ms")
  }

  test("testing parsing") {
    val startTime = System.currentTimeMillis()
    testCykAccepting()
    val endTime = System.currentTimeMillis()
    println(s"time used: ${endTime - startTime} ms")
  }

  private def testChomsky(): Unit =
    val rule1 = ContextFreeProductionRule("S -> A + B")
    val rule2 = ContextFreeProductionRule("A -> 1")
    val rule3 = ContextFreeProductionRule("B -> C * D")
    val rule4 = ContextFreeProductionRule("C -> 2")
    val rule5 = ContextFreeProductionRule("D -> 3333")
    val cfg = ContextFreeGrammar("S", List(rule1, rule2, rule3, rule4, rule5))

    val cyk = CykAlgorithm(cfg) // will throw an exception

  private def testChomsky2(): Unit =
    val rule1 = ContextFreeProductionRule("S -> A B")
    val rule2 = ContextFreeProductionRule("A -> 1")
    val rule3 = ContextFreeProductionRule("B -> C D")
    val rule4 = ContextFreeProductionRule("C -> 2")
    val rule5 = ContextFreeProductionRule("D -> 3333")
    val cfg = ContextFreeGrammar("S", List(rule1, rule2, rule3, rule4, rule5))

    val cyk = CykAlgorithm(cfg) // will compile

  private def testCykAccepting(): Unit =
    val rule1 = ContextFreeProductionRule("S -> A B")
    val rule2 = ContextFreeProductionRule("A -> 1")
    val rule3 = ContextFreeProductionRule("B -> B C")
    val rule4 = ContextFreeProductionRule("C -> D E")
    val rule5 = ContextFreeProductionRule("D -> 3333")
    val rule6 = ContextFreeProductionRule("E -> 2")
    val cfg = ContextFreeGrammar("S", List(rule1, rule2, rule3, rule4, rule5))
    val cyk = CykAlgorithm(cfg)
}
