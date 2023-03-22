package com.compilers.demo.parsing

class CommonASTNode(val symbol: String,
                    val children: List[CommonASTNode] = List.empty,
                    val isTerminal: Boolean = false):

  override def clone(): CommonASTNode =
    if this != ErrorASTNode then
      new CommonASTNode(this.symbol, this.children.map(_.clone()), this.isTerminal)
    else
      ErrorASTNode

/**
 * indicating a parsing error
 */
object ErrorASTNode extends CommonASTNode("$$$Error!", List.empty, true)
