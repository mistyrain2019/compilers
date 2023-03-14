package com.compilers.demo.parsing

import scala.collection.immutable

class ContextFreeGrammar private():

  var startSymbol: String = ""

  var symbols: immutable.Set[String] = immutable.Set()

  var nonTerminals: immutable.Set[String] = immutable.Set()

  var terminals: immutable.Set[String] = immutable.Set()

  var rules: List[ContextFreeProductionRule] = List()

  override def toString: String =
    rules.map(_.toString).mkString("\n")

object ContextFreeGrammar:

  def apply(start: String, ruleList: List[ContextFreeProductionRule]): ContextFreeGrammar =
    val cfg = new ContextFreeGrammar()
    cfg.startSymbol = start
    cfg.rules = ruleList

    cfg.symbols = (ruleList.map(_.leftHandSide) ++ ruleList.flatMap(_.rightHandSide)).distinct.toSet
    cfg.nonTerminals = cfg.symbols.filter(s => GrammarUtil.isNonTerminal(s))
    cfg.terminals = cfg.symbols.filter(s => GrammarUtil.isTerminal(s) || GrammarUtil.isEpsilon(s))
    cfg