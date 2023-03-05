package com.compilers.demo

import com.compilers.demo.regex.RegexCompileStrategy.EpsilonNFAStrategy
import com.compilers.demo.regex.RegexMatcher

object App {
  def main(args: Array[String]): Unit = {
    testMatch()
  }

  private def testMatch(): Unit =
    val regex = RegexMatcher.compile("u?a?b+(cd*|esdfs*f)*gh..k", EpsilonNFAStrategy)

    println(regex.matches("uabbbbbbbbesdfsssfesdfssssssfghabk")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijk")) // false
}
