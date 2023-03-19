package com.compilers.demo.regex

import com.compilers.demo.regex.{EpsilonNFA, RegexParser}
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