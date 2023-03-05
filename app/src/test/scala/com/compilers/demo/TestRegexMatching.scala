package com.compilers.demo


import com.compilers.demo.regex.RegexCompileStrategy.*
import com.compilers.demo.regex.RegexMatcher
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestRegexMatching extends AnyFunSuite {
  test("test regex matching") {
    val initTime = System.currentTimeMillis()

    testMatching()

    val endTime = System.currentTimeMillis()
    println(s"the duration is ${endTime - initTime} ms")
  }


  private def testMatching(): Unit =

    val regex = RegexMatcher.compile("u?a?b+(cd*|esdfs*f)*gh..k", EpsilonNFAStrategy)

    println(regex.matches("uabbbbbbbbesdfsssfesdfssssssfghabk")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijk")) // false
}