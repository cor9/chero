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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference
import scalafx.beans.Observable

object ObservableIntervalMap {

  def apply[A, B](implicit ordering: Ordering[A]) = new ObservableIntervalMap[A, B]()

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

  case class MoveChange[A, B](from: (A, A), to: (A, A), elem: B) extends Change[A, B]


  case class RemoveChange[A, B](removed: List[(A, A, B)]) extends Change[A, B]

}

class ObservableIntervalMap[A, B](implicit ordering: Ordering[A]) extends Observable with Traversable[(A, A, B)] {
  private val starts: mutable.SortedMap[A, (A, A, B)] = mutable.TreeMap()
  private val ends: mutable.SortedMap[A, (A, A, B)] = mutable.TreeMap()
  private var invalidationListeners = Set[InvalidationListener]()
  private var listeners = Set[ObservableIntervalMap.ChangeListener[A, B]]()

  def apply(a: A): Option[(A, A, B)] = {
    val start = starts.to(a).lastOption.map(_._2)
    val end = ends.from(a).headOption.map(_._2._2)
    start.filter(s => end.contains(s._2))
  }

  def apply(startA: A, endA: A): List[(A, A, B)] = {
    val start = starts.to(endA).iterator.map(_._2)
    val end = ends.from(startA)
    start.filter(s => end.contains(s._2)).toList
  }

  def starting(startA: A, endA: A): List[(A, A, B)] = {
    val start = starts.to(endA).iterator.map(_._2)
    val end = starts.from(startA)
    start.filter(s => end.contains(s._2)).toList
  }

  def ending(startA: A, endA: A): List[(A, A, B)] = {
    val start = starts.to(endA).iterator.map(_._2)
    val end = starts.from(startA)
    start.filter(s => end.contains(s._2)).toList
  }

  def intersecting(startA: A, endA: A): List[(A, A, B)] = {
    val start = starting(startA, endA)
    val end = ending(startA, endA).headOption.filterNot(e => start.headOption.exists(s => e._1 == s._1))
    end.toList ++ start
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

  def move(from: (A, A), to: (A, A)): Unit = {
    require(intersecting(from._1, from._2).filterNot(e => e._1 == from._1).isEmpty)
    val b = starts(from._1)._3
    starts -= from._1
    ends -= from._2
    starts(to._1) = (from._1, from._2, b)
    ends(to._2) = (from._1, from._2, b)
    for (listener <- listeners) {
      listener.onChange(this, ObservableIntervalMap.MoveChange(from, to, b))
    }
    for (listener <- invalidationListeners) {
      listener.invalidated(this)
    }
  }

  def ++=(elems: TraversableOnce[(A, A, B)]): Unit = {
    val list = elems.toList
    require(list.forall(e => intersecting(e._1, e._2).isEmpty))
    for ((startA, endA, b) <- elems) {
      starts(startA) = (startA, endA, b)
      ends(startA) = (startA, endA, b)
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
