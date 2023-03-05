package com.compilers.demo.regex

import com.compilers.demo.regex.State


class Transition(
  val transitionCondition: TransitionCondition,
  val targetState: State
)
