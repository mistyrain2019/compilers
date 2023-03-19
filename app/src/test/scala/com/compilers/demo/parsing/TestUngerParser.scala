package com.compilers.demo.parsing

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestUngerParser extends AnyFunSuite {

  test("testing generator") {
    testSearchingNodeGenerator()
  }

  test("testing parsing") {
    testUngerParsingMethod()
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
}
