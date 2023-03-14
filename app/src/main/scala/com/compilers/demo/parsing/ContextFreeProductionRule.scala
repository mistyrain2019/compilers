package com.compilers.demo.parsing

import com.compilers.demo.parsing.ContextFreeProductionRule.raiseError

class ContextFreeProductionRule(lhs: String, rhs: List[String]):

  var leftHandSide: String = ""
  var rightHandSide: List[String] = List()

  if !GrammarUtil.isNonTerminal(lhs.trim) then
    raiseError("LeftHandSide must be non-terminal in a Context-Free Production Rule!")

  leftHandSide = lhs.trim

  rightHandSide = if rhs.exists(str => !GrammarUtil.isEpsilon(str)) then
    rhs.filterNot(GrammarUtil.isEpsilon)
  else
    List("")


  override def toString: String =
    if isEpsilonProduction then
      s"$leftHandSide -> ε"
    else
      s"$leftHandSide -> ${rightHandSide.mkString(" ")}"

  def isEpsilonProduction: Boolean =
    rightHandSide.length == 1 && rightHandSide.head.isEmpty

object ContextFreeProductionRule:

  def apply(rule: String): ContextFreeProductionRule =

    if rule.countSubStringOccurrences("->") != 1 then
      raiseError("illegal production rule!")

    if rule.indexOf("->") == 0 then
      raiseError("leftHandSide could not be empty!")

    val splitRule = rule.split("->")

    if splitRule.length != 1 && splitRule.length != 2 then
      raiseError("illegal production rule!")

    val lhs = splitRule.head.trim
    if !GrammarUtil.isNonTerminal(lhs) then
      raiseError("leftHandSide must be NonTerminal!")

    var rhs: List[String] = List("") // default value is epsilon rule

    if splitRule.length == 2 then
      val right = splitRule(1).trim.split(" ").filter(_.nonEmpty).toList
      if right.nonEmpty then
        rhs = right

    new ContextFreeProductionRule(lhs, rhs)

  def raiseError(msg: String = ""): Unit =
    throw ContextFreeProductionRuleException(msg)

  private class ContextFreeProductionRuleException(msg: String) extends RuntimeException(msg)
