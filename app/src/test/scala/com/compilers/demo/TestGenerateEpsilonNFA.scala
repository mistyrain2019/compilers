package com.compilers.demo

import com.compilers.demo.regex.RegexParser
import com.compilers.demo.regex.EpsilonNFA
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestGenerateEpsilonNFA extends AnyFunSuite {
  test("test generate epsilon nfa") {
    val ast = RegexParser("ab(c|f)g*h*").parseNormalPattern()
    println(ast.preTraverse())
    
    val nfa = EpsilonNFA.thompsonConstruction(ast)
    println(nfa)
  }
}