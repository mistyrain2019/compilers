package com.compilers.demo.parsing

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestContextFreeProductionRule extends AnyFunSuite {
  test("parsing CFS rules") {
    testProductionRule()
    testProductionError()
  }

  private def testProductionRule(): Unit =
    val productionRule = ContextFreeProductionRule("A ->")
    println(productionRule)


    val p1 = ContextFreeProductionRule("K ->   ")
    println(p1)
    assert(p1.isEpsilonProduction)

    val p2 = ContextFreeProductionRule("O ->  ε ")
    println(p2)
    assert(p2.isEpsilonProduction)

    val p3 = ContextFreeProductionRule("Bcd ->ε")
    println(p3)
    assert(p3.isEpsilonProduction)

    val p4 = ContextFreeProductionRule("T ->  a  c   d  B  ")
    println(p4)
    assert(!p4.isEpsilonProduction)

  private def testProductionError(): Unit =
//    ContextFreeProductionRule("A -> s -> k")
//    ContextFreeProductionRule("A ->  -> k")
//    ContextFreeProductionRule("abb -> k dsd")
    ContextFreeProductionRule(" -> k dsd")
}
