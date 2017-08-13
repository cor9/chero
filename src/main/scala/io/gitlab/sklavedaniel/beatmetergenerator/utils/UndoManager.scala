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

import scala.collection.JavaConverters
import scalafx.beans.property.{Property, ReadOnlyBooleanWrapper}
import scalafx.beans.value.ObservableValue
import scalafx.collections.ObservableBuffer

object UndoManager {
  def register[A, B](undoManager: Option[UndoManager], property: Property[A, B]) = {
    undoManager.foreach(um => {
      property.onChange(um.propertyAction(property))
    })
  }

  def register[A](undoManager: Option[UndoManager], list: ObservableBuffer[A]) = {
    undoManager.foreach(um => {
      list.onChange(um.listAction(list))
    })
  }

  def register[A, B](undoManager: Option[UndoManager], map: ObservableIntervalMap[A, B]) = {
    undoManager.foreach(um => {
      map.addListener(um.intervalMapAction(map))
    })
  }
}

final class UndoManager {

  private var undoActions = List[(() => Unit, () => Unit)]()
  private var redoActions = List[(() => Unit, () => Unit)]()
  var active = true
  private var group: Option[List[(() => Unit, () => Unit)]] = None

  def clear(): Unit = {
    undoActions = Nil
    redoActions = Nil
    group = None
    undoable_() = false
    redoable_() = false
  }

  def startGroup(): Unit = {
    group = Some(Nil)
  }

  def endGroup(): Unit = {
    val list = group.get
    group = None
    if (list.nonEmpty) {
      doAction(() => {
        list.reverse.foreach(_._1())
      }, () => {
        list.foreach(_._2())
      })
    }
  }

  def propertyAction[A, B](property: Property[A, B]) = (_: ObservableValue[A, B], old: B, current: B) => {
    if (old != current) {
      doAction(() => {
        property.setValue(current)
      }, () => {
        property.setValue(old)
      })
    }
  }

  def listAction[A](list: ObservableBuffer[A]) = (_: ObservableBuffer[A], cs: Seq[ObservableBuffer.Change[A]]) => {
    if (cs.nonEmpty) {
      val changes = for (c <- cs) yield
        c match {
          case ObservableBuffer.Add(pos, elems) =>
            Left((pos, JavaConverters.asJavaCollection(elems.toList)))
          case ObservableBuffer.Remove(pos, elems) =>
            Right((pos, JavaConverters.asJavaCollection(elems.toList)))
          case _ => assert(false); ???
        }
      doAction(() => {
        for (c <- changes) {
          c match {
            case Left((pos, elems)) =>
              list.addAll(pos, elems)
            case Right((pos, elems)) =>
              list.remove(pos, elems.size)
          }
        }
      }, () => {
        for (c <- changes.reverse) {
          c match {
            case Left((pos, elems)) =>
              list.remove(pos, elems.size)
            case Right((pos, elems)) =>
              list.addAll(pos, elems)
            case _ => assert(false)
          }
        }
      })
    }
  }

  def intervalMapAction[A, B](map: ObservableIntervalMap[A, B]): ObservableIntervalMap.ChangeListener[A, B] = (_: ObservableIntervalMap[A, B], change: ObservableIntervalMap.Change[A, B]) => {
    change match {
      case ObservableIntervalMap.AddChange(cs) =>
        if (cs.nonEmpty) {
          doAction(() => {
            map ++= cs
          }, () => {
            map --= cs.map(c => (c._1, c._2))
          })
        }
      case ObservableIntervalMap.RemoveChange(cs) => {
        if (cs.nonEmpty) {
          doAction(() => {
            map --= cs.map(c => (c._1, c._2))
          }, () => {
            map ++= cs
          })
        }
      }
      case ObservableIntervalMap.MoveChange(from, to, cs) =>
        if (cs.nonEmpty) {
          doAction(() => {
            map --= cs.map(c => c._1)
            map ++= cs.map(c => (c._2._1, c._2._2, c._3))
          }, () => {
            map --= cs.map(c => c._2)
            map ++= cs.map(c => (c._1._1, c._1._2, c._3))
          })
        }
    }
  }

  def doAction(undo: () => Unit, redo: () => Unit): Unit = {
    if (active) {
      if (group.isDefined) {
        group = Some((undo, redo) :: group.get)
      } else {
        undoActions = (undo, redo) :: undoActions
        redoActions = Nil
        redoable_() = false
        undoable_() = true
      }
    }
  }

  def undoAction(): Unit = {
    assert(group.isEmpty)
    if (undoActions.nonEmpty) {
      active = false
      val action = undoActions.head
      undoActions = undoActions.tail
      redoActions = action :: redoActions
      action._2()
      redoable_() = true
      undoable_() = undoActions.nonEmpty
      active = true
    }
  }

  def redoAction(): Unit = {
    assert(group.isEmpty)
    if (redoActions.nonEmpty) {
      active = false
      val action = redoActions.head
      redoActions = redoActions.tail
      undoActions = action :: undoActions
      action._1()
      redoable_() = redoActions.nonEmpty
      undoable_() = true
      active = true
    }
  }

  private val undoable_ = ReadOnlyBooleanWrapper(false)
  val undoable = undoable_.readOnlyProperty
  private val redoable_ = ReadOnlyBooleanWrapper(false)
  val redoable = redoable_.readOnlyProperty

}
