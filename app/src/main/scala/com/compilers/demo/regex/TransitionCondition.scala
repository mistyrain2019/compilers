package com.compilers.demo.regex

import scala.collection.mutable


sealed trait TransitionCondition {

  def accept(character: Char): Boolean

  def acceptEpsilon: Boolean = false
}

case class SpecificCharTransitionCondition(c: Char) extends TransitionCondition:

  def accept(character: Char): Boolean = character == c


case object AnyCharTransitionCondition extends TransitionCondition:

  def accept(character: Char): Boolean = true


case object EpsilonTransitionCondition extends TransitionCondition:

  def accept(character: Char): Boolean = false

  override def acceptEpsilon = true


object TransitionCondition:

  private val charTransitionConditionPool = mutable.Map[Char, SpecificCharTransitionCondition]()

  for c <- 'a' to 'z' do
    charTransitionConditionPool(c) = SpecificCharTransitionCondition(c)

  for c <- 'A' to 'Z' do
    charTransitionConditionPool(c) = SpecificCharTransitionCondition(c)

  for c <- '0' to '9' do
    charTransitionConditionPool(c) = SpecificCharTransitionCondition(c)


  def getCharTransition(char: Char): SpecificCharTransitionCondition =
    if !charTransitionConditionPool.contains(char) then
      charTransitionConditionPool(char) = SpecificCharTransitionCondition(char)

    charTransitionConditionPool(char)



