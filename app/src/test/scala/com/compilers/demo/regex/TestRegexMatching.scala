package com.compilers.demo.regex

import com.compilers.demo.regex.RegexMatcher
import com.compilers.demo.regex.RegexMatcher.*
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestRegexMatching extends AnyFunSuite {
  test("test regex matching") {
    val initTime = System.currentTimeMillis()

    //    testMatching()
    //    testMatching2()
    testMatchingByStandardLibrary()

    val endTime = System.currentTimeMillis()
    println(s"the duration is ${endTime - initTime} ms")
  }


  private def testMatching(): Unit =

    val regex = RegexMatcher.compile("u?a?b+(cd*|esdfs*f)*gh..k", RegexCompileStrategy.EpsilonNFAStrategy)

    println(regex.matches("uabbbbbbbbesdfsssfesdfssssssfghabk")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijk")) // false

  private def testMatching2(): Unit =

    val regex = RegexMatcher.compile("(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)*a?u+t*y*(r|sr*)?", RegexCompileStrategy.EpsilonNFAStrategy)

    println(regex.matches("weiuyrweuirhewuirhweiuhauuuuyyyyysrrrrr")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijku")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijkurb")) // false

  private def testMatchingByStandardLibrary(): Unit =
    val regex = "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)*a?u+t*y*(r|sr*)?".r

    println(regex.matches("weiuyrweuirhewuirhweiuhauuuuyyyyysrrrrr")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijku")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijkurb")) // false
}