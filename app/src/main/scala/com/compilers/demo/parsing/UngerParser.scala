package com.compilers.demo.parsing


import com.compilers.demo.parsing.UngerParser.EPSILON_SYMBOL_LIST
import com.compilers.demo.parsing.UngerParser.mapEmptySymbolsToEpsilon

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class UngerParser(val cfg: ContextFreeGrammar):

  def parse(inputSymbolStream: List[String]): CommonASTNode =
    doMatch(cfg.startSymbol, inputSymbolStream, mutable.HashMap(), mutable.Set())

  private def doMatch(matchingLeftHandSide: String,
                      symbols: List[String],
                      memo: mutable.Map[String, CommonASTNode],
                      matchInProcessPatterns: mutable.Set[String]): CommonASTNode =

    val key = s"$matchingLeftHandSide@${symbols.mkString("#")}"

    if memo.contains(key) then
      return memo(key).clone()

    else if cfg.terminals.contains(matchingLeftHandSide) then
      if symbols.size == 1 && symbols.head == matchingLeftHandSide then
        memo(key) = CommonASTNode(matchingLeftHandSide)
        return memo(key)
      else
        return ErrorASTNode

    if matchInProcessPatterns.contains(key) then // cut off searching patterns
      return ErrorASTNode

    matchInProcessPatterns.add(key)

    val suitedRules = cfg.rules.filter(_.leftHandSide == matchingLeftHandSide)


    def doSearchList(nodes: List[SearchingStateNode],
                     rightHandSide: List[String],
                     pos: Int,
                     buffer: ListBuffer[CommonASTNode]
                    ): List[CommonASTNode] =

      if pos == rightHandSide.size then
        return buffer.toList
      for node <- nodes do
        val aSTNode = doMatch(rightHandSide(pos), node.symbols, memo, matchInProcessPatterns)
        if aSTNode != ErrorASTNode then
          buffer.addOne(aSTNode)
          val res = doSearchList(node.children, rightHandSide, pos + 1, buffer)
          if res.size == rightHandSide.size then
            return res
          buffer.remove(buffer.size - 1)
      List()


    for rule <- suitedRules do
      val rightHandSymbolsCount = rule.rightHandSide.size
      val partitionedList = generateSearchingNode(symbols, rightHandSymbolsCount)
      val searchedAstNodes = doSearchList(partitionedList, rule.rightHandSide, 0, ListBuffer())
      if searchedAstNodes.size == rightHandSymbolsCount then
        val successNode = CommonASTNode(matchingLeftHandSide, searchedAstNodes)
        memo(key) = successNode

        matchInProcessPatterns.remove(key)
        return successNode

    matchInProcessPatterns.remove(key)
    ErrorASTNode


  def generateSearchingNode(symbolsToPartition: List[String], count: Int): List[SearchingStateNode] =
    if count == 0 then
      return List()
    else if count == 1 then
      return List(SearchingStateNode(mapEmptySymbolsToEpsilon(symbolsToPartition)))

    val buffer = ListBuffer[SearchingStateNode]()

    for i <- 0 to symbolsToPartition.size do
      val node = SearchingStateNode(mapEmptySymbolsToEpsilon(symbolsToPartition.slice(0, i))) // leftmost part of a partition
      node.children = generateSearchingNode(symbolsToPartition.slice(i, symbolsToPartition.size), count - 1) // use recursive to deal with the right
      buffer.addOne(node)

    buffer.toList


  case class SearchingStateNode(symbols: List[String]):
    var children: List[SearchingStateNode] = List()


object UngerParser:

  val EPSILON_SYMBOL_LIST: List[String] = List("")

  def mapEmptySymbolsToEpsilon(symbols: List[String]): List[String] =
    if symbols.isEmpty then EPSILON_SYMBOL_LIST else symbols



