package com.compilers.demo.parsing


import com.compilers.demo.parsing.UngerParser.EPSILON_SYMBOL_LIST
import com.compilers.demo.parsing.UngerParser.mapEmptySymbolsToEpsilon
import com.compilers.demo.parsing.UngerParser.SearchingStateNode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
        return recordAndReturn(CommonASTNode(matchingLeftHandSide, List.empty, true))
      else
        return recordAndReturn(ErrorASTNode)

    if matchInProcessPatterns.contains(key) then // cut off now-searching patterns
      return ErrorASTNode

    matchInProcessPatterns.add(key)

    val suitedRules = cfg.rules.filter(_.leftHandSide == matchingLeftHandSide)

    var ifSucceed = false
    var resAstNode: CommonASTNode = ErrorASTNode

    for rule <- suitedRules if !ifSucceed do
      val partitionedList = generateSearchingNode(symbols, rule.rightHandSide.size)

      val searchedAstNodes = doSearchList(partitionedList, rule.rightHandSide, 0, ListBuffer(), memo)

      if searchedAstNodes.size == rule.rightHandSide.size then
        resAstNode = CommonASTNode(matchingLeftHandSide, searchedAstNodes)
        ifSucceed = true

    matchInProcessPatterns.remove(key)
    recordAndReturn(resAstNode)

  private def doSearchList(nodes: List[SearchingStateNode],
                           rightHandSide: List[String],
                           pos: Int,
                           buffer: ListBuffer[CommonASTNode],
                           memo: mutable.Map[String, CommonASTNode],
                          ): List[CommonASTNode] =

    if pos == rightHandSide.size then
      return buffer.toList

    var res: List[CommonASTNode] = List.empty
    var succeed = false

    for node <- nodes if !succeed do
      val astNodeAtPos = doMatch(rightHandSide(pos), node.symbols, memo)
      if astNodeAtPos != ErrorASTNode then // one matched
        buffer.addOne(astNodeAtPos)

        val astNodesList = doSearchList(node.children, rightHandSide, pos + 1, buffer, memo) // recursive searching (pos + 1)
        if astNodesList.size == rightHandSide.size then
          succeed = true
          res = astNodesList

        buffer.remove(buffer.size - 1)

    res


  def generateSearchingNode(symbolsToPartition: List[String], count: Int): List[SearchingStateNode] =
    if count == 0 then
      return List.empty
    else if count == 1 then
      return List(SearchingStateNode(mapEmptySymbolsToEpsilon(symbolsToPartition)))

    val buffer = ListBuffer[SearchingStateNode]()

    for i <- 0 to symbolsToPartition.size do
      val node = SearchingStateNode(mapEmptySymbolsToEpsilon(symbolsToPartition.slice(0, i))) // leftmost part of a partition
      node.children = generateSearchingNode(symbolsToPartition.slice(i, symbolsToPartition.size), count - 1) // use recursive to deal with the right
      buffer.addOne(node)

    buffer.toList


object UngerParser:

  case class SearchingStateNode(symbols: List[String]):
    var children: List[SearchingStateNode] = List.empty

  val EPSILON_SYMBOL_LIST: List[String] = List("")

  def mapEmptySymbolsToEpsilon(symbols: List[String]): List[String] =
    if symbols.isEmpty then EPSILON_SYMBOL_LIST else symbols

