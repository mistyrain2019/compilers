package com.compilers.demo.regex


import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class State:
  var id: Int = State.generateId()
  var isFinal = false
  var transitions: ListBuffer[Transition] = ListBuffer()

object State:
  private var id_count = 0

  private def generateId(): Int =
    id_count += 1
    id_count
