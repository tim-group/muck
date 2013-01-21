package com.youdevise.muck

import org.specs2.mutable._
import org.specs2.specification.{Given, When, Then}

import Muck._

object SlotsSpec extends Specification {
  """A slot""" should {
    """be readable""" in {
      val theString = slot[String]
      val map = Shelf(theString -> "Hello World")
      (map \\ theString) must beEqualTo("Hello World")
    }

    """be writable""" in {
      val theString = slot[String]
      val map = Shelf(theString -> "Hello World")
      val newMap = map.set(theString, "Goodbye, Cruel World")
      (newMap \\ theString) must beEqualTo("Goodbye, Cruel World")
    }

    """be implicitly convertable into a slot reader""" in {
      val theString = slot[String]
      val reader = for {
        value <- theString
      } yield value
      val result = reader ! Shelf(theString -> "Hello Monadic World")
      result must beEqualTo("Hello Monadic World")
    }

    """take an assignment and return a slot writer""" in {
      val theString = slot[String]
      val reader = for {
        value <- theString := "Goodbye, Monadic World"
      } yield value
      val (shelf, result) = reader(Shelf(theString -> "Hello Monadic World"))
      shelf must beEqualTo(Shelf(theString -> "Goodbye, Monadic World"))
      result must beEqualTo("Goodbye, Monadic World")
    }
  }
}
