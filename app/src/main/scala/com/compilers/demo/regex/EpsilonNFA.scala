package com.compilers.demo.regex

import scala.collection.mutable


class EpsilonNFA extends RegexMatcher:
  private var start: State = State()
  private var end: State = State()
  end.isFinal = true


  override def matches(content: String): Boolean =
    doMatch(content, 0, this.start, mutable.Set())

  private def doMatch(content: String, ptr: Int, state: State, memo: mutable.Set[String]): Boolean =

    def generateKey(): String = s"${state.id}#@%${ptr}"

    def checkVisited(): Boolean = memo.contains(generateKey())

    def setVisited(): Unit = memo.add(generateKey())

    if ptr == content.length && state == this.end then
      return true

    if checkVisited() then
      return false
    else
      setVisited()

    if state.transitions
      .filter(_.transitionCondition.acceptEpsilon)
      .exists(transition => doMatch(content, ptr, transition.targetState, memo)) then
      return true

    if state.transitions
      .filter(_.transitionCondition.accept(content.charAt(ptr)))
      .exists(transition => doMatch(content, ptr + 1, transition.targetState, memo)) then
      return true

    false

object EpsilonNFA:

  def thompsonConstruction(ast: RegexASTNode): EpsilonNFA =
    ast match
      case expression: Expression =>
        if expression.children.size == 3 then // alternation
          assert(expression.children(1).asInstanceOf[RegexToken].char == '|')
          val nfa1 = thompsonConstruction(expression.children(0))
          val nfa2 = thompsonConstruction(expression.children(2))
          return thompsonUnion(nfa1, nfa2)

        if expression.children.size == 1 then
          return thompsonConstruction(expression.children(0))

        reportError()

      case term: Term =>
        if term.children.size == 2 then // concatenation
          val nfa1 = thompsonConstruction(term.children(0))
          val nfa2 = thompsonConstruction(term.children(1))
          return thompsonConcatenation(nfa1, nfa2)

        else if term.children.size == 1 then
          return thompsonConstruction(term.children(0))

        reportError()

      case word: Word =>
        if word.children.size == 2 then
          assert(word.children(1).isInstanceOf[Suffix])
          val suffixNode = word.children(1)

          assert(suffixNode.children.size == 1)
          val operatorChar = suffixNode.children(0).asInstanceOf[RegexToken].char

          if operatorChar == '*' then
            val childNfa = thompsonConstruction(word.children(0))
            return thompsonKleeneStar(childNfa)

          else if operatorChar == '+' then
            val childNfa = thompsonConstruction(word.children(0))
            return oneOrMore(childNfa)

          else if operatorChar == '?' then
            val childNfa = thompsonConstruction(word.children(0))
            return zeroOrOne(childNfa)

        else if word.children.size == 1 then
          return thompsonConstruction(word.children(0))

        reportError()

      case root: Root =>
        if root.children.size == 3 then
          assert(root.children(0).asInstanceOf[RegexToken].char == '(')
          assert(root.children(2).asInstanceOf[RegexToken].char == ')')
          return thompsonConstruction(root.children(1))

        else if root.children.size == 1 then
          return thompsonConstruction(root.children(0))

        reportError()

      case character: CommonCharacter =>
        commonSymbol(character.char)

      case suffix: Suffix => // should not be processed at this level
        reportError()

      case token: RegexToken => // should not be processed at this level
        reportError()

      case _ =>
        reportError()

  /**
   * '|' operator
   */
  private def thompsonUnion(nfa1: EpsilonNFA, nfa2: EpsilonNFA): EpsilonNFA =
    val res = new EpsilonNFA()
    addEpsilonTransition(res.start, nfa1.start)
    addEpsilonTransition(res.start, nfa2.start)

    nfa1.end.isFinal = false
    nfa2.end.isFinal = false

    addEpsilonTransition(nfa1.end, res.end)
    addEpsilonTransition(nfa2.end, res.end)

    res

  /**
   * concatenation
   */
  private def thompsonConcatenation(nfa1: EpsilonNFA, nfa2: EpsilonNFA): EpsilonNFA =
    nfa1.end.transitions = nfa2.start.transitions
    nfa2.start = nfa1.end
    nfa1.end = nfa2.end
    nfa1

  /**
   * '*' operator
   */
  private def thompsonKleeneStar(nfa: EpsilonNFA): EpsilonNFA =
    val res = EpsilonNFA()
    addEpsilonTransition(res.start, res.end)
    addEpsilonTransition(res.start, nfa.start)
    addEpsilonTransition(nfa.end, res.end)
    addEpsilonTransition(nfa.end, nfa.start)
    res

  /**
   * '+' operator
   */
  private def oneOrMore(nfa: EpsilonNFA): EpsilonNFA =
    val res = new EpsilonNFA()
    addEpsilonTransition(res.start, nfa.start)
    addEpsilonTransition(nfa.end, res.end)
    addEpsilonTransition(nfa.end, nfa.start)
    res

  /**
   * '?' operator
   */
  private def zeroOrOne(nfa: EpsilonNFA): EpsilonNFA =
    val res = EpsilonNFA()
    addEpsilonTransition(res.start, res.end)
    addEpsilonTransition(res.start, nfa.start)
    addEpsilonTransition(nfa.end, res.end)
    res

  private def commonSymbol(c: Char): EpsilonNFA =
    val res = EpsilonNFA()

    val transitionCondition = if c == '.' then
      AnyCharTransitionCondition
    else
      TransitionCondition.getCharTransition(c)

    val transition = Transition(transitionCondition, res.end)
    res.start.transitions += transition
    res


  private def addEpsilonTransition(from: State, to: State): Unit =
    from.transitions += Transition(EpsilonTransitionCondition, to)


  private def reportError(): EpsilonNFA =
    throw RuntimeException("Construction Error!")

