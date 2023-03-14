package com.compilers.demo.parsing

class ContextFreeProductionRule(lhs: String, rhs: List[String]):

  var leftHandSide: String = ""
  var rightHandSide: List[String] = List()

  assert(GrammarUtil.isNonTerminal(lhs))
  assert(rhs.count(str => GrammarUtil.isEpsilon(str)) <= 1)

  if rhs.isEmpty || (rhs.length == 1 && GrammarUtil.isEpsilon(rhs.head)) then
    rightHandSide = List("")
  else
    rightHandSide = rhs

  leftHandSide = lhs


  override def toString: String =
    if isEpsilonProduction then
      s"$leftHandSide -> ε"
    else
      s"$leftHandSide -> ${rightHandSide.mkString(" ")}"

  def isEpsilonProduction: Boolean =
    rightHandSide.length == 1 && rightHandSide.head.isEmpty

object ContextFreeProductionRule:
  def apply(rule: String): ContextFreeProductionRule =
    assert(rule.countSubStringOccurrences("->") == 1)
    val split = rule.split("->")

    assert(split.length == 1 || split.length == 2)

    val lhs = split.head.trim
    assert(GrammarUtil.isNonTerminal(lhs))

    val rhs: List[String] = if split.length == 1 then
      List("")
    else
      val right = split(1).trim.split(" ").filter(_.nonEmpty)
      if right.isEmpty then
        List("")
      else
        right.toList

    new ContextFreeProductionRule(lhs, rhs)


