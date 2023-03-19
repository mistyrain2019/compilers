package com.compilers.demo

import com.compilers.demo.regex.RegexMatcher.*
import com.compilers.demo.regex.RegexMatcher
import com.compilers.demo.regex.EpsilonNFA

object App {
  def main(args: Array[String]): Unit = {
    testMatch()
  }

  private def testMatch(): Unit =
    val regex = RegexMatcher.compile("(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)*a?u+t*y*(r|sr*)?", RegexCompileStrategy.EpsilonNFAStrategy)

    println(regex.matches("weiuyrweuirhewuirhweiuhauuuuyyyyysrrrrr")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijku")) // true
    println(regex.matches("abbbbbbbbesdfsssfesdfsffffsssssfghijkurb")) // false
}
