package com.compilers.demo.regex

import RegexMatcher.RegexCompileStrategy.{EpsilonNFAStrategy, NFAStrategy, DFAStrategy}


trait RegexMatcher:

  def matches(content: String): Boolean


object RegexMatcher:

  def compile(pattern: String, strategy: RegexCompileStrategy = EpsilonNFAStrategy): RegexMatcher =
    val ast = RegexParser(inputPattern = pattern).parseNormalPattern()
    strategy match
      case EpsilonNFAStrategy => EpsilonNFA.thompsonConstruction(ast)
      case NFAStrategy => throw new RuntimeException()
      case DFAStrategy => throw new RuntimeException()

  enum RegexCompileStrategy:
    case EpsilonNFAStrategy, NFAStrategy, DFAStrategy