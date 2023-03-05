package com.compilers.demo.regex


import scala.collection.{immutable, mutable}

/**
 * convert a regex to an AST
 *
 * supported regex rules:   | * ? + . ( )
 *
 *
 * using a LL(1) grammar to represent the language, which is:
 *
 * -------------------------------------------------------------------------------------------
 * |    Expression -> Term | Term '|' Expression    // Alternation
 * |
 * |    Term -> Word Term | Word                    // Concatenation
 * |
 * |    Word -> Root | Root Suffix                  // Other operators, like repetition
 * |
 * |    Suffix -> * | ? | +                         // Repetition, Zero-or-one, One-or-more
 * |
 * |    Root -> ( Expression ) | character          // Parentheses
 * -------------------------------------------------------------------------------------------
 *
 * build the AST through Recursive Descent parsing technique
 *
 */
class RegexParser(val inputPattern: String = ""):

  if inputPattern.isEmpty then
    raiseError()

  private val pattern = inputPattern

  private var rootNode: RegexASTNode = null

  private var completed = false


  def parseNormalPattern(): RegexASTNode =
    if !completed then
      rootNode = recursiveDescentParsing()
      completed = true

    rootNode


  private var currentPosition = 0

  private def currentChar(): Char = pattern.charAt(currentPosition)

  private def next(): Unit = currentPosition += 1

  private def isPointerOutOfRange: Boolean = currentPosition >= pattern.length


  private def recursiveDescentParsing(): RegexASTNode =
    val node = matchExpression()
    node


  private def matchExpression(): RegexASTNode =
    val term = matchTerm()

    if !isPointerOutOfRange && currentChar() == '|' then
      val alter = matchKeyword('|')
      val expression = matchExpression()
      Expression(children = List(term, alter, expression))
    else
      Expression(children = List(term))

  private def matchTerm(): RegexASTNode =
    val word = matchWord()

    if !isPointerOutOfRange && currentChar() != ')' && currentChar() != '|' then
      val term = matchTerm()
      Term(children = List(word, term))
    else
      Term(children = List(word))

  private def matchWord(): RegexASTNode =
    val root = matchRoot()

    if !isPointerOutOfRange && RegexParser.isSuffix(currentChar()) then
      val suffix = matchSuffix()
      Word(children = List(root, suffix))
    else
      Word(children = List(root))

  private def matchRoot(): RegexASTNode =
    if currentChar() == '(' then
      val l = matchKeyword('(')
      val expressionNode = matchExpression()
      val r = matchKeyword(')')
      Root(children = List(l, expressionNode, r))
    else
      Root(children = List(matchCharacter()))

  private def matchSuffix(): RegexASTNode =
    val c = currentChar()
    if !RegexParser.isSuffix(c) then
      raiseError()

    Suffix(children = List(matchKeyword(c)))


  private def matchKeyword(c: Char): RegexASTNode =
    if isPointerOutOfRange || c != currentChar() || !RegexParser.isKeyword(c) then
      raiseError()

    next()
    RegexToken(char = c)

  private def matchCharacter(): RegexASTNode =
    if isPointerOutOfRange then
      raiseError()

    val c = currentChar()
    if !RegexParser.isCharacter(c) then
      raiseError()

    next()
    CommonCharacter(char = c)

  private def raiseError(): Unit =
    completed = true
    rootNode = null
    throw RuntimeException("syntax error!")


object RegexParser:

  private val keywords = immutable.Set[Char]('|', '*', '?', '+', '(', ')')

  private val suffix = immutable.Set[Char]('*', '?', '+')

  private def isCharacter(c: Char): Boolean = !keywords.contains(c)

  private def isKeyword(c: Char): Boolean = keywords.contains(c)

  private def isSuffix(c: Char): Boolean = suffix.contains(c)