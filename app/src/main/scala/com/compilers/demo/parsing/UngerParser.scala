package com.compilers.demo.parsing


import com.compilers.demo.parsing.UngerParser.EPSILON_SYMBOL_LIST
import com.compilers.demo.parsing.UngerParser.mapEmptySymbolsToEpsilon
import com.compilers.demo.parsing.UngerParser.SearchingStateNode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 *
 * @param cfg whose rules should not contain any '#' or '@' character
 */
class UngerParser(val cfg: ContextFreeGrammar):

  private val matchInProcessPatterns: mutable.Set[String] = mutable.HashSet()

  def parse(inputSymbolStream: List[String]): CommonASTNode =
    doMatch(cfg.startSymbol, inputSymbolStream, mutable.HashMap())

  private def doMatch(matchingLeftHandSide: String,
                      symbols: List[String],
                      memo: mutable.Map[String, CommonASTNode]): CommonASTNode =

    val key = s"$matchingLeftHandSide@${symbols.mkString("#")}"

    if memo.contains(key) then // find same pattern in the memo
      return memo(key).clone()

    inline def recordAndReturn(astNode: CommonASTNode): CommonASTNode =
      memo(key) = astNode
      astNode

    if cfg.terminals.contains(matchingLeftHandSide) then
      if symbols.size == 1 && symbols.head == matchingLeftHandSide then
        return recordAndReturn(CommonASTNode(matchingLeftHandSide, Nil, true))
      else
        return recordAndReturn(ErrorASTNode)

    if matchInProcessPatterns.contains(key) then // cut off now-searching patterns
      return ErrorASTNode

    matchInProcessPatterns.add(key)

    val suitedRules = cfg.rules.filter(_.leftHandSide == matchingLeftHandSide)

    var ifSucceed = false
    var resAstNode: CommonASTNode = ErrorASTNode

    for rule <- suitedRules if !ifSucceed do
      val searchedAstNodes = searchRightHandSide(symbols, rule, 0, mutable.ListBuffer(), memo) // match Rhs

      if searchedAstNodes.size == rule.rightHandSide.size then // successfully matched
        resAstNode = CommonASTNode(symbol = matchingLeftHandSide, children = searchedAstNodes, isTerminal = false)
        ifSucceed = true

    matchInProcessPatterns.remove(key)
    recordAndReturn(resAstNode)

  private def searchRightHandSide(symbols: List[String],
                                  rule: ContextFreeProductionRule,
                                  posOfRightHandSide: Int,
                                  buffer: ListBuffer[CommonASTNode],
                                  memo: mutable.Map[String, CommonASTNode]
                                 ): List[CommonASTNode] =

    if posOfRightHandSide == rule.rightHandSide.size then // all Rhs matches
      return if symbols.isEmpty || (symbols.size == 1 && GrammarUtil.isEpsilon(symbols.head)) then // check whether there is symbols left
        buffer.toList
      else
        Nil

    var res: List[CommonASTNode] = Nil

    for i <- 0 to symbols.size if res == Nil do // search every partition

      val leftMostOfRhs = mapEmptySymbolsToEpsilon(symbols.slice(0, i)) // leftmost part, now searching

      val leftMostAstNode = doMatch(rule.rightHandSide(posOfRightHandSide), leftMostOfRhs, memo) // check if it matches with the pos of Rhs

      if leftMostAstNode != ErrorASTNode then // leftmost part matched at pos

        val restPartOfRhs: List[String] = mapEmptySymbolsToEpsilon(symbols.slice(i, symbols.size)) // left part

        buffer.addOne(leftMostAstNode)

        val astList = searchRightHandSide(restPartOfRhs, rule, posOfRightHandSide + 1, buffer, memo) // recursively dealing with next pos

        if astList.size == rule.rightHandSide.size then // since we only add non-Error node to the buffer, the same size means matching successfully
          res = astList

        buffer.remove(buffer.size - 1)
    res


object UngerParser:

  case class SearchingStateNode(symbols: List[String]):
    var children: List[SearchingStateNode] = Nil

  val EPSILON_SYMBOL_LIST: List[String] = List("")

  def mapEmptySymbolsToEpsilon(symbols: List[String]): List[String] =
    if symbols.isEmpty then
      EPSILON_SYMBOL_LIST
    else
      symbols

