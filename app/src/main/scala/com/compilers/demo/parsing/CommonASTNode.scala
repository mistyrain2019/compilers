package com.compilers.demo.parsing

class CommonASTNode(val symbol: String,
                    val children: List[CommonASTNode] = List(),
                    val isTerminal: Boolean = false):

  override def clone(): CommonASTNode =
    new CommonASTNode(this.symbol, this.children.map(_.clone()), this.isTerminal)


/**
 * indicating a parsing error
 */
object ErrorASTNode extends CommonASTNode("$$$Error!", List(), true)