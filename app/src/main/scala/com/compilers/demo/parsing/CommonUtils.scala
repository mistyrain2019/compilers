package com.compilers.demo.parsing

object GrammarUtil:

  def isEpsilon(str: String): Boolean =
    str == null || str.trim.isEmpty || str.trim.equals("epsilon") || str.trim.equals("ε")

  def isNonTerminal(str: String): Boolean =
    str != null && str.nonEmpty && str.head.isUpper && !str.contains(" ")

  def isTerminal(str: String): Boolean =
    str != null && str.nonEmpty && !str.head.isUpper && !str.contains(" ")


extension (str: String)
  def countSubStringOccurrences(subStr: String): Int =
    str.sliding(subStr.length).count(win => win == subStr)