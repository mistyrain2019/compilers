package com.compilers.demo.parsing


import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class UngerParser(val cfg: ContextFreeGrammar):

  def parse(inputSymbolStream: List[String]): CommonASTNode =
    doMatch(cfg.startSymbol, inputSymbolStream, mutable.HashMap())

  private def doMatch(matchingLeftHandSide: String,
                      symbols: List[String],
                      memo: mutable.Map[String, CommonASTNode]): CommonASTNode =

    val key = s"$matchingLeftHandSide@${symbols.mkString("#")}"
    if memo.contains(key) then
      return memo(key).clone()

    else if cfg.terminals.contains(matchingLeftHandSide) then
      if symbols.size == 1 && symbols.head == matchingLeftHandSide then
        memo(key) = CommonASTNode(matchingLeftHandSide)
        return memo(key)
      else
        return ErrorASTNode

    val suitedRules = cfg.rules.filter(_.leftHandSide == matchingLeftHandSide)

    for rule <- suitedRules do
      val rightHandSymbolsCount = rule.rightHandSide.size
      val partitionedList = generateSearchingNode(symbols, rightHandSymbolsCount)
      val searchedAstNodes = doSearchList(partitionedList, rule.rightHandSide, 0, ListBuffer(), memo)
      if searchedAstNodes.size == rightHandSymbolsCount then
        val successNode = CommonASTNode(matchingLeftHandSide, searchedAstNodes)
        memo(key) = successNode
        return successNode

    ErrorASTNode

  def doSearchList(nodes: List[SearchingStateNode],
                   rightHandSide: List[String],
                   pos: Int,
                   buffer: ListBuffer[CommonASTNode],
                   memo: mutable.Map[String, CommonASTNode]): List[CommonASTNode] =

    if pos == rightHandSide.size then
      return buffer.toList
    for node <- nodes do
      val aSTNode = doMatch(rightHandSide(pos), node.symbols, memo)
      if aSTNode != ErrorASTNode then
        buffer.addOne(aSTNode)
        val res = doSearchList(node.children, rightHandSide, pos + 1, buffer, memo)
        if res.size == rightHandSide.size then
          return res
        buffer.remove(buffer.size - 1)
    List()

  def generateSearchingNode(symbolsToPartition: List[String], count: Int): List[SearchingStateNode] =
    if count == 0 then
      return List()
    else if count == 1 then
      return List(SearchingStateNode(symbolsToPartition.emptyOrElse(List(""))))

    val buffer = ListBuffer[SearchingStateNode]()
    val node0 = SearchingStateNode(List(""))
    node0.children = generateSearchingNode(symbolsToPartition, count - 1)
    buffer.addOne(node0)

    for i <- symbolsToPartition.indices do
      val node = SearchingStateNode(symbolsToPartition.slice(0, i + 1))
      node.children = generateSearchingNode(symbolsToPartition.slice(i + 1, symbolsToPartition.size), count - 1)
      buffer.addOne(node)

    buffer.toList


  case class SearchingStateNode(symbols: List[String] = List()):
    var children: List[SearchingStateNode] = List()


