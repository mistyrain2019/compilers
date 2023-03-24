package com.compilers.demo.parsing

import com.compilers.demo.parsing.CykAlgorithm.NonChomskyException


/**
 * can be used to check if a String lists belong to the language of a CFG
 *
 * At this time, we do not use this class to generate AST.
 *
 * @param cfg must be Chomsky Normal Form (that is, only two kinds of rules are allow, which is S -> A B / C -> d)
 */
class CykAlgorithm(val cfg: ContextFreeGrammar):

  checkChomsky() // throw an exception if cfg is not in Chomsky Normal Form
  private def checkChomsky(): Unit =

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


  def accept(inputSymbols: List[String]): Boolean =
    false

object CykAlgorithm:

  private class NonChomskyException(msg: String) extends RuntimeException(msg)