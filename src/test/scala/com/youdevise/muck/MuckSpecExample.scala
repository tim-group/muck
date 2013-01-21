package com.youdevise.muck

import Muck._
import org.specs2._
import specification.{Given, When, Then}

class MuckSpecExample extends Specification {
  val firstString = slot[String]
  val secondString = slot[String]
  val concatenatedString = slot[String]

  def is =
  "First String concatenation example" ^
    "Given that I have two strings"             ^ makeTwoStrings ^
    "When I concatenate them"                   ^ concatenateTheStrings ^
    "Then I have a single concatenated string"  ^ testConcatenatedString ^
                                                end ^
  "Second String concatenation example" ^
    "Given that I have a string ${monster }"    ^ makeFirstString ^
    "and another string ${munch}"               ^ makeSecondString ^
    "When I concatenate them"                   ^ concatenateTheStrings ^
    "Then I have the result ${monster munch}"   ^ testSpecificConcatenatedString ^ end

  val makeTwoStrings = for {
    _ <- firstString := "Hello "
    _ <- secondString := "World"
  } yield givens

  val concatenateTheStrings = for {
    s1 <- firstString
    s2 <- secondString
    _  <- concatenatedString := (s1 + s2)
  } yield whens

  val testConcatenatedString = for {
    result <- concatenatedString
  } yield {
    result must beEqualTo("Hello World")
  }

  def arg(s: String) = """\$\{(.*)\}""".r.findFirstMatchIn(s).get.subgroups(0)

  val makeFirstString = { s: String => for {
    _ <- firstString := arg(s)
  } yield givens }

  val makeSecondString = { s: String => for {
    _ <- secondString := arg(s)
  } yield givens }

  val testSpecificConcatenatedString: Then[Shelf] = { s: String =>
  for {
    result <- concatenatedString
  } yield {
    result must beEqualTo(arg(s))
  } }

}
