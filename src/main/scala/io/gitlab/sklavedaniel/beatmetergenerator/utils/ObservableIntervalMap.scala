/*
 *  This file is part of Beatmeter Generator.
 *
 *  Beatmeter Generator is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Beatmeter Generator is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Beatmeter Generator.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package io.gitlab.sklavedaniel.beatmetergenerator.utils

import javafx.beans
import javafx.beans.{InvalidationListener, WeakListener}

import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap.Unscalable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference
import scalafx.beans.Observable

object ObservableIntervalMap {

  trait Unscalable[A] {
    def duration: A
  }

  def apply[A, B](implicit fractional: Fractional[A]) = new ObservableIntervalMap[A, B]()

  trait ChangeListener[A, B] {
    def onChange(map: ObservableIntervalMap[A, B], change: Change[A, B])
  }

  def WeakChangeListner[A, B](listener: ChangeListener[A, B]) = new ChangeListener[A, B] with WeakListener {
    private val reference = WeakReference(listener)

    override def onChange(map: ObservableIntervalMap[A, B], change: Change[A, B]): Unit = {
      reference.get match {
        case Some(listener) => listener.onChange(map, change)
        case None => map.removeListener(listener)
      }
    }

    override def wasGarbageCollected: Boolean = reference.get.isEmpty
  }

  sealed trait Change[A, B]

  case class AddChange[A, B](added: List[(A, A, B)]) extends Change[A, B]

  case class MoveChange[A, B](from: (A, A), to: (A, A), moved: List[((A, A), (A, A), B)]) extends Change[A, B]

  case class RemoveChange[A, B](removed: List[(A, A, B)]) extends Change[A, B]

}

class ObservableIntervalMap[A, B](implicit fractional: Fractional[A]) extends Observable with Traversable[(A, A, B)] {

  import fractional.mkNumericOps
  import fractional.mkOrderingOps

  private val starts: mutable.SortedMap[A, (A, A, B)] = mutable.TreeMap()
  private val ends: mutable.SortedMap[A, (A, A, B)] = mutable.TreeMap()
  private var invalidationListeners = Set[InvalidationListener]()
  private var listeners = Set[ObservableIntervalMap.ChangeListener[A, B]]()

  def apply(a: A): Option[(A, A, B)] =
    ends.from(a).headOption.map(_._2).filter(_._1 <= a)

  def increase(a: A): Option[(A, A, B)] =
    starts.from(a).take(2).values.find(_._1 > a)

  def decrease(a: A): Option[(A, A, B)] =
    starts.to(a).takeRight(2).values.find(_._1 < a)

  def apply(startA: A, endA: A): List[(A, A, B)] =
    starts.from(startA).iterator.map(_._2).takeWhile(_._2 <= endA).toList


  def starting(startA: A, endA: A): List[(A, A, B)] =
    starts.from(startA).iterator.takeWhile(_._1 <= endA).map(_._2).toList

  def ending(startA: A, endA: A): List[(A, A, B)] =
    ends.from(startA).iterator.takeWhile(_._1 <= endA).map(_._2).toList

  def intersecting(startA: A, endA: A): List[(A, A, B)] = {
    ends.from(startA).iterator.map(_._2).takeWhile(_._1 <= endA).toList
  }

  def within(startA: A, endA: A): List[(A, A, B)] = {
    starts.from(startA).iterator.map(_._2).takeWhile(_._2 <= endA).toList
  }

  def --=(elems: TraversableOnce[(A, A)]): List[(A, A, B)] = {
    val result = ListBuffer[(A, A, B)]()
    for ((startA, endA) <- elems; (startA2, endA2, b) <- apply(startA, endA)) {
      starts -= startA2
      ends -= endA2
      result += ((startA2, endA2, b))
    }
    val list = result.toList
    for (listener <- listeners) {
      listener.onChange(this, ObservableIntervalMap.RemoveChange(list))
    }
    for (listener <- invalidationListeners) {
      listener.invalidated(this)
    }
    list
  }

  def clear(): List[(A, A, B)] = {
    val list = starts.values.toList
    starts.clear()
    ends.clear()
    for (listener <- listeners) {
      listener.onChange(this, ObservableIntervalMap.RemoveChange(list))
    }
    for (listener <- invalidationListeners) {
      listener.invalidated(this)
    }
    list
  }

  override def lastOption = starts.lastOption.map(_._2)

  override def last = lastOption.get

  def canMove(from: (A, A), to: (A, A)) = {
    val scale = (to._2 - to._1) / (from._2 - from._1)
    intersecting(to._1, to._2).forall(e => from._1 <= e._1 && e._2 <= from._2) &&
      intersecting(from._1, from._2).sliding(2).forall {
        case List(first, second) if first._3.isInstanceOf[Unscalable[A]] =>
          to._1 + scale * (first._1 - from._1) + first._3.asInstanceOf[Unscalable[A]].duration < to._1 + scale * (second._1 - from._1)
        case _ => true
      }
  }

  def move(from: (A, A), to: (A, A)): Unit = {
    require(canMove(from, to))
    if (from != to) {
      val scale = (to._2 - to._1) / (from._2 - from._1)
      val list = ListBuffer[((A, A), (A, A), B)]()
      for (elem <- intersecting(from._1, from._2)) {
        val b = starts(elem._1)._3
        starts -= elem._1
        ends -= elem._2
        val toStart = to._1 + scale * (elem._1 - from._1)
        val toEnd = if (!b.isInstanceOf[Unscalable[A]]) {
          if (elem._2 == from._2) {
            to._2
          } else {
            to._1 + scale * (elem._2 - from._1)
          }
        } else {
          to._1 + scale * (elem._1 - from._1) + b.asInstanceOf[Unscalable[A]].duration
        }
        starts(toStart) = (toStart, toEnd, b)
        ends(toEnd) = (toStart, toEnd, b)
        list += (((elem._1, elem._2), (toStart, toEnd), b))
      }
      for (listener <- listeners) {
        listener.onChange(this, ObservableIntervalMap.MoveChange(from, to, list.toList))
      }
      for (listener <- invalidationListeners) {
        listener.invalidated(this)
      }
    }
  }

  def contains(startA: A, endA: A) = starts.from(startA).headOption.exists(_ == endA)

  def ++=(elems: TraversableOnce[(A, A, B)]): Unit = {
    val list = elems.toList
    require(list.forall(e => intersecting(e._1, if(e._3.isInstanceOf[Unscalable[A]]) e._1 + e._3.asInstanceOf[Unscalable[A]].duration else e._2).isEmpty))
    for ((startA, endA, b) <- list) {
      val tmp = if(b.isInstanceOf[Unscalable[A]]) startA + b.asInstanceOf[Unscalable[A]].duration else endA
      starts(startA) = (startA, tmp, b)
      ends(tmp) = (startA, tmp, b)
    }
    for (listener <- listeners) {
      listener.onChange(this, ObservableIntervalMap.AddChange(list))
    }
    for (listener <- invalidationListeners) {
      listener.invalidated(this)
    }
  }

  override def foreach[U](f: ((A, A, B)) => U): Unit = {
    starts.values.foreach(f)
  }

  def invalidate(): Unit = {
    invalidationListeners.foreach(_.invalidated(this))
  }

  def addListener(listener: ObservableIntervalMap.ChangeListener[A, B]): Unit = {
    listeners = trim(listeners) + listener
  }


  def removeListener(listener: ObservableIntervalMap.ChangeListener[A, B]): Unit = {
    listeners = trim(listeners) + listener
  }

  override val delegate = new beans.Observable {
    override def removeListener(listener: InvalidationListener): Unit = {
      invalidationListeners -= listener
    }

    override def addListener(listener: InvalidationListener): Unit = {
      invalidationListeners += listener
    }
  }

  private def trim[T](list: Set[T]): Set[T] =
    list.filter {
      case (elem: WeakListener) => !elem.wasGarbageCollected()
      case _ => true
    }
}
