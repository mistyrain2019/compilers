package com.compilers.demo.regex


sealed abstract class RegexASTNode(val children: List[RegexASTNode]):
  def preTraverse(): String


sealed class NonTerminal(label: String, override val children: List[RegexASTNode]) extends RegexASTNode(children):
  override def preTraverse(): String = s"Non-terminal node: ${label}🔽\n ${children.map(_.preTraverse()).mkString(" ### ")}"


sealed class Terminal(char: Char) extends RegexASTNode(List()):
  override def preTraverse(): String = s"Terminal node: $char"


// Non-Terminals
case class Suffix(override val children: List[RegexASTNode]) extends NonTerminal("Suffix", children)

case class Root(override val children: List[RegexASTNode]) extends NonTerminal("Root", children)

case class Word(override val children: List[RegexASTNode]) extends NonTerminal("Word", children)

case class Term(override val children: List[RegexASTNode]) extends NonTerminal("Term", children)

case class Expression(override val children: List[RegexASTNode]) extends NonTerminal("Expression", children)


// Terminals
case class CommonCharacter(char: Char) extends Terminal(char)

case class RegexToken(char: Char) extends Terminal(char)
