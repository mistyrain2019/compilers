package com.compilers.demo.parsing

import com.compilers.demo.parsing.CykMethod.checkChomsky

import scala.collection.mutable


/**
 * can be used to check if a String lists belong to the language of a CFG
 *
 * At this time, we do not use this class to generate AST.
 *
 * @param cfg must be Chomsky Normal Form (that is, only two kinds of rules are allow, which is S -> A B / C -> d)
 */
class CykMethod(val cfg: ContextFreeGrammar):

  checkChomsky(this.cfg) // throw an exception if cfg is not in Chomsky Normal Form

  def accept(inputSymbols: List[String]): Boolean =
    val n = inputSymbols.size

    val r = Array.ofDim[mutable.Set[String]](n, n + 1)
    for i <- 0 until n do
      for j <- 0 to n do
        r(i)(j) = mutable.Set[String]()

    val size2Rules = cfg.rules.filter(_.rightHandSide.size == 2)
    val size1Rules = cfg.rules.filter(_.rightHandSide.size == 1)

    for i <- inputSymbols.indices do
      if cfg.terminals.contains(inputSymbols(i)) then
        for left <- size1Rules.filter(_.rightHandSide.head == inputSymbols(i)).map(_.leftHandSide) do
          r(i)(1).add(left)


    for j <- 2 to n do
      for i <- 0 to (n - j) do
        for k <- 1 until j do
          val r1: mutable.Set[String] = r(i)(k)
          val r2: mutable.Set[String] = r(i + k)(j - k)
          for (rule <- size2Rules) do {
            val leftNonTerminal = rule.rightHandSide(0)
            val rightNonTerminal = rule.rightHandSide(1)
            if (r1.contains(leftNonTerminal) && r2.contains(rightNonTerminal)) {
              r(i)(j).add(rule.leftHandSide)
            }
          }

    r(0)(n).contains(cfg.startSymbol)

object CykMethod:

  private class NonChomskyException(msg: String) extends RuntimeException(msg)

  private def checkChomsky(cfg: ContextFreeGrammar): Unit =

    val rightHandSides = cfg.rules.map(_.rightHandSide)

    if rightHandSides.flatten.exists(_.isEmpty) then
      throw NonChomskyException("epsilon rules are not allowed in a Chomsky Normal Form!")

    if rightHandSides.exists(_.isEmpty) then
      throw NonChomskyException("epsilon rules are not allowed in a Chomsky Normal Form!")

    if rightHandSides.exists(_.size > 2) then
      throw NonChomskyException("rhs size > 2 is not allowed in a Chomsky Normal Form!")

    val sizeOneRhs: List[List[String]] = rightHandSides.filter(_.size == 1)

    if sizeOneRhs.flatten.exists(str => GrammarUtil.isNonTerminal(str)) then
      throw NonChomskyException("Unit Rule is not allowed in a Chomsky Normal Form!")

    val sizeTwoRhs: List[List[String]] = rightHandSides.filter(_.size == 2)

    if sizeTwoRhs.flatten.exists(GrammarUtil.isTerminal) then
      throw NonChomskyException("size 2 Rhs' containing terminals is not allowed in a Chomsky Normal Form!")

    if rightHandSides.contains(cfg.startSymbol) then
      throw NonChomskyException("size 2 Rhs' containing the start symbol is not allowed in a Chomsky Normal Form!")