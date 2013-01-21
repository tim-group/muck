package com.youdevise.muck

import scalaz._
import Scalaz._
import org.specs2.specification.{Given, When, Then}
import org.specs2.execute.{Result, ResultLike}

sealed case class Shelf(val innerMap: Map[Slot[_], Any]) {
  def get[T](slot: Slot[T]): Option[T] = innerMap.get(slot).map(_.asInstanceOf[T])
  def getOrElse[T](slot: Slot[T], default: => T) = get(slot).getOrElse(default)
  def \[T](slot: Slot[T]): Option[T] = get(slot)
  def \\[T](slot: Slot[T]): T = get(slot).get
  def set[T](slot: Slot[T], value: T): Shelf = Shelf(innerMap ++ Seq(slot -> value))
  def combine(other: Shelf) = Shelf(innerMap ++ other.innerMap)
}

object Shelf {
  def apply(values: (Slot[_], Any)*): Shelf = Shelf(values.toMap)
  def combineAll(shelves: Iterable[Shelf]): Shelf = shelves.reduce { _ combine _ }
}

sealed class Slot[T]() { }

object Muck {

  type ShelfState[T] = State[Shelf, T]
  type ShelfStateMaker[T] = (String) => ShelfState[T]

  sealed case class Givens()
  sealed case class Whens()

  val givens = Givens()
  val whens = Whens()

  def slot[T] = new Slot[T]

  implicit def slot2Reader[T](slot: Slot[T]): State[Shelf, T] = state[Shelf, T] { shelf:Shelf =>
    (shelf, (shelf \\ slot))
  }

  sealed class ExtendedSlot[T](val slot: Slot[T]) {
    def :=(value: T): State[Shelf, T] = state[Shelf, T] { shelf: Shelf =>
      (shelf.set(slot, value), value)
    }

    def or(value: T): State[Shelf, T] = state[Shelf, T] { shelf: Shelf =>
      (shelf, (shelf \ slot).getOrElse(value))
    }
  }

  implicit def slot2Extended[T](slot: Slot[T]): ExtendedSlot[T] = new ExtendedSlot(slot)

  implicit def given(shelfStateMaker: ShelfStateMaker[Givens]): Given[Shelf] = new Given[Shelf] {
    def extract(text: String) = shelfStateMaker(text)(Shelf())._1
  }

  implicit def given(shelfState: ShelfState[Givens]): Given[Shelf] = given { _: String => shelfState }

  implicit def when(shelfStateMaker: ShelfStateMaker[Whens]): When[Shelf, Shelf] = new When[Shelf, Shelf] {
    def extract(shelf: Shelf, text: String) = shelfStateMaker(text)(shelf)._1
  }

  implicit def when(shelfState: ShelfState[Whens]): When[Shelf, Shelf] = when { _: String => shelfState }

  implicit def when2(shelfStateMaker: ShelfStateMaker[Whens]): When[(Shelf, Shelf), Shelf] = new When[(Shelf, Shelf), Shelf] {
    def extract(shelves: (Shelf, Shelf), text: String) = shelfStateMaker(text)(shelves._1 combine shelves._2)._1
  }

  implicit def when2(shelfState: ShelfState[Whens]): When[(Shelf, Shelf), Shelf] = when2 { _: String => shelfState }

  implicit def when3(shelfStateMaker: ShelfStateMaker[Whens]): When[(Shelf, Shelf, Shelf), Shelf] = new When[(Shelf, Shelf, Shelf), Shelf] {
    def extract(shelves: (Shelf, Shelf, Shelf), text: String) = shelfStateMaker(text)(Shelf.combineAll(shelves.toIndexedSeq))._1
  }

  implicit def when3(shelfState: ShelfState[Whens]): When[(Shelf, Shelf, Shelf), Shelf] = when3 { _: String => shelfState }

  implicit def then(shelfStateMaker: ShelfStateMaker[ResultLike]): Then[Shelf] = new Then[Shelf] {
    def extract(shelf: Shelf, text: String): Result = shelfStateMaker(text)(shelf)._2.toResult
  }

  implicit def then(shelfState: ShelfState[ResultLike]): Then[Shelf] = then { _: String => shelfState }
}