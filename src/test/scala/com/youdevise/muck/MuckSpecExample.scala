package com.youdevise.muck

import Muck._
import org.specs2._
import specification.{Given, When, Then}

class MuckSpecExample extends Specification {
  val firstString = slot[String]
  val secondString = slot[String]
  val thirdString = slot[String]
  val concatenatedString = slot[String]

  def is =
  "First String concatenation example" ^
    "Given that I have two strings"             ^ makeTwoStrings ^
    "When I concatenate them"                   ^ concatenateTheStrings ^
    "Then I have a single concatenated string"  ^ testConcatenatedString ^
                                                end ^
  "Second String concatenation example"         ^
    "Given that I have a string ${monster }"    ^ populate(firstString) ^
    "and another string ${munch}"               ^ populate(secondString) ^
    "When I concatenate them"                   ^ concatenateTheStrings ^
    "Then I have the result ${monster munch}"   ^ testSpecificConcatenatedString ^
                                                end ^
  "Third String concatenation example"          ^
    "Given that I have a string ${red}"         ^ populate(firstString) ^
    "and another string ${rum}"                 ^ populate(secondString) ^
    "When I concatenate them"                   ^ concatenateTheStrings ^
    "And reverse them"                          ^ reverseThe(concatenatedString) ^
    "Then I have the result ${murder}"          ^ testSpecificConcatenatedString ^
                                                end ^
  "Fourth String concatenation example"         ^
    "Given that I have a string ${fee }"        ^ populate(firstString) ^
    "and another string ${fi }"                 ^ populate(secondString) ^
    "and yet another string ${fum}"             ^ populate(thirdString) ^
    "When I reverse the second"                 ^ reverseThe(secondString) ^
    "And concatenate them"                      ^ concatenateTheStrings ^
    "Then I have the result ${fee  iffum}"      ^ testSpecificConcatenatedString ^
                                                end

  val makeTwoStrings = for {
    _ <- firstString := "Hello "
    _ <- secondString := "World"
  } yield givens

  val concatenateTheStrings = for {
    s1 <- firstString
    s2 <- secondString
    s3 <- thirdString or ""
    _  <- concatenatedString := (s1 + s2 + s3)
  } yield whens

  val testConcatenatedString = for {
    result <- concatenatedString
  } yield {
    result must beEqualTo("Hello World")
  }

  def arg(s: String) = """\$\{(.*)\}""".r.findFirstMatchIn(s).get.subgroups(0)

  def populate(slot: Slot[String]) = { s: String => for {
    _ <- slot := arg(s)
  } yield givens }

  val testSpecificConcatenatedString: Then[Shelf] = { s: String =>
  for {
    result <- concatenatedString
  } yield {
    result must beEqualTo(arg(s))
  } }

  def reverseThe(slot: Slot[String]) = for {
    s <- slot
    _ <- slot := s.reverse
  } yield whens

}
