package com.compilers.demo.parsing

object GrammarUtil:

  /**
   *  ""(empty string), "epsilon", and "ε" are all regarded as the epsilon grammar in a CFG
   */
  def isEpsilon(str: String): Boolean =
    str == null || str.trim.isEmpty || str.trim.equals("epsilon") || str.trim.equals("ε")

  /**
   *  Non-terminals must start with an uppercase charater
   */
  def isNonTerminal(str: String): Boolean =
    str != null && str.nonEmpty && str.head.isUpper && !str.contains(" ")

  /**
   *  terminals must not start with an uppercase charater
   */
  def isTerminal(str: String): Boolean =
    str != null && str.nonEmpty && !str.head.isUpper && !str.contains(" ")


extension (str: String)
  def countSubStringOccurrences(subStr: String): Int =
    str.sliding(subStr.length).count(window => window == subStr)
