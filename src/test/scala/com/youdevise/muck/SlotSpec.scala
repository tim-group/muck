package com.youdevise.muck

import org.specs2.mutable._

sealed case class SlottedMap(val innerMap: Map[Slot[_], Any]) {
  def get[T](slot: Slot[T]): Option[T] = innerMap.get(slot).map(_.asInstanceOf[T])
  def getOrElse[T](slot: Slot[T], default: => T) = get(slot).getOrElse(default)
  def \[T](slot: Slot[T]): Option[T] = get(slot)
  def \\[T](slot: Slot[T]): T = get(slot).get
  def set[T](slot: Slot[T], value: T): SlottedMap = SlottedMap(innerMap ++ Seq(slot -> value))
}

object SlottedMap {
  def apply(values: (Slot[_], Any)*): SlottedMap = SlottedMap(values.toMap)
}

sealed class Slot[T]() { }

object Slots {
  def slot[T] = new Slot[T]
}

import Slots._

object SlotsSpec extends Specification {
  """A slot""" should {
    """be readable""" in {
      val theString = slot[String]
      val map = SlottedMap(theString -> "Hello World")
      (map \\ theString) must beEqualTo("Hello World")
    }
  }
}
