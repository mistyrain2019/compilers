package com.compilers.demo.parsing

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestUngerParser extends AnyFunSuite {

  test("testing parsing") {
    val startTime = System.currentTimeMillis()
    //    testSearchingNodeGenerator()
    //    testUngerParsingMethod()
    testUngerParsingMethod2()
    val endTime = System.currentTimeMillis()
    println(s"time used: ${endTime - startTime} ms")
  }

  private def testSearchingNodeGenerator(): Unit =
    val strList = List("aaaa", "b", "c")
    val nodeList = UngerParser(null).generateSearchingNode(strList, 4)
    println(nodeList)
    var searching = nodeList(0)
    for i <- 0 until 3 do
      searching = searching.children(0)

    assert(searching.children.isEmpty)

  private def testUngerParsingMethod(): Unit =
    val rule1 = ContextFreeProductionRule("S -> A + B")
    val rule2 = ContextFreeProductionRule("A -> 1")
    val rule3 = ContextFreeProductionRule("B -> C E * D F")
    val rule4 = ContextFreeProductionRule("C -> 2")
    val rule5 = ContextFreeProductionRule("D -> 3333")
    val rule6 = ContextFreeProductionRule("E -> epsilon")
    val rule7 = ContextFreeProductionRule("F -> epsilon")
    val cfg = ContextFreeGrammar("S", List(rule1, rule2, rule3, rule4, rule5, rule6, rule7))

    val ungerParsingMethod = UngerParser(cfg)

    val symbols = List("1", "+", "2", "*", "3333")
    val ast = ungerParsingMethod.parse(symbols)
    println(ast)
    assert(ast != ErrorASTNode)

  private def testUngerParsingMethod2(): Unit =
    val rule1 = ContextFreeProductionRule("Expr -> Expr + Term")
    val rule2 = ContextFreeProductionRule("Expr -> Term")
    val rule3 = ContextFreeProductionRule("Term -> Term * Factor")
    val rule4 = ContextFreeProductionRule("Term -> Factor")
    val rule5 = ContextFreeProductionRule("Factor -> ( Expr )")
    val rule6 = ContextFreeProductionRule("Factor -> i")
    val cfg = ContextFreeGrammar("Expr", List(rule1, rule2, rule3, rule4, rule5, rule6))

    val ungerParsingMethod = UngerParser(cfg)

    val symbols = List("(", "i", "+", "i", ")", "*", "(", "i", "*", "i", ")")
    val ast = ungerParsingMethod.parse(symbols)
    println(ast)
    assert(ast != ErrorASTNode)
}
