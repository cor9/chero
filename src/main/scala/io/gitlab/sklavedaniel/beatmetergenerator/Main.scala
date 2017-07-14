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

package io.gitlab.sklavedaniel.beatmetergenerator

import java.awt.Color
import java.util.Locale

import org.rogach.scallop.{ArgType, ScallopConf, Subcommand, ValueConverter}
import shapeless.{HList, HNil, :: => :::}

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

object Main extends App {

  Locale.setDefault(Locale.ROOT)

  trait Converters {
    trait StringParser[A] {
      def parse(s: String): A
    }

    val ColorRegex = "((?:[0-9a-fA-F]{2})?[0-9a-fA-F]{6})".r
    implicit val colorConverter = new ValueConverter[Color] {
      override def parse(s: List[(String, List[String])]) = s match {
        case List((_, List(ColorRegex(color)))) =>
          val i = Integer.parseUnsignedInt(color, 16)
          Right(Some(new Color(i, (i >>> 24) != 0)))
        case Nil => Right(None)
        case _ => Left("Not a correct color string")
      }

      val tag = scala.reflect.runtime.universe.typeTag[Color]
      val argType = ArgType.SINGLE
    }

    implicit def optionConverter[A: TypeTag](implicit conv: ValueConverter[A]) = new ValueConverter[Option[A]] {
      def parse(s: List[(String, List[String])]): Either[String, Option[Option[A]]] = {
        s match {
          case Nil => Right(None)
          case List((_, Nil)) => Right(Some(None))
          case List((_, l)) => conv.parse(List(("", l))).map(Some(_))
          case _ => Left("wrong arguments format")
        }
      }

      val tag = scala.reflect.runtime.universe.typeTag[Option[A]]
      val argType = ArgType.LIST
    }

    implicit def listConverter[A: TypeTag](implicit convA: ValueConverter[A]) =
      new ValueConverter[List[A]] {
        def parse(s: List[(String, List[String])]): Either[String, Option[List[A]]] = {
          s match {
            case List((_, l)) =>
              getGroup(l) match {
                case Right((Nil, Nil)) => Right(None)
                case Right((head, tail)) => for {
                  ar <- convA.parse(List(("", head)))
                  br <- parse(List(("", tail)))
                } yield for {
                  a <- ar
                  b <- br
                } yield a :: b
                case _ => Left("wrong arguments format")
              }
            case Nil => Right(None)
            case _ => Left("wrong arguments format")
          }
        }

        val tag = scala.reflect.runtime.universe.typeTag[List[A]]
        val argType = ArgType.LIST
      }

    implicit val hnilConverter = new ValueConverter[HNil] {
      def parse(s: List[(String, List[String])]): Either[String, Option[HNil]] = {
        s match {
          case Nil => Right(None)
          case List((_, Nil)) => Right(Some(HNil))
          case _ => Left("wrong arguments format")
        }
      }

      val tag = scala.reflect.runtime.universe.typeTag[HNil]
      val argType = ArgType.LIST
    }

    implicit def hlistConverter[A: TypeTag, B <: HList : TypeTag](implicit convA: ValueConverter[A], convB: ValueConverter[B]) =
      new ValueConverter[A ::: B] {
        def parse(s: List[(String, List[String])]): Either[String, Option[A ::: B]] = {
          s match {
            case List((_, l)) =>
              getGroup(l) match {
                case Right((Nil, Nil)) => Right(None)
                case Right((head, tail)) =>
                  for {
                    ar <- convA.parse(List(("", head)))
                    br <- convB.parse(List(("", tail)))
                  } yield for {
                    a <- ar
                    b <- br
                  } yield a :: b
                case Left(s) =>
                  Left(s)
                case _ =>
                  Left("wrong arguments format")
              }
            case Nil => Right(None)
            case x =>
              Left("wrong arguments format")
          }
        }

        val tag = scala.reflect.runtime.universe.typeTag[A ::: B]
        val argType = ArgType.LIST
      }
  }

  abstract class ExecutableSubcommand(name: String) extends Subcommand(name) with Converters {

    def execute(subcommands: List[ScallopConf], args: Array[String])
  }

  def getGroup(list: List[String]): Either[String, (List[String], List[String])] = {
    list match {
      case "(" :: tail =>
        getParGroup(tail) match {
          case Left(s) => Left(s)
          case Right((result, rest)) => Right((result, rest))
        }
      case ")" :: tail =>
        Left("unexpected \")\"")
      case s :: tail =>
        Right(List(s), tail)
      case Nil =>
        Right((Nil, Nil))
    }
  }

  def getParGroup(list: List[String]): Either[String, (List[String], List[String])] =
    getParGroups(list, Nil)


  @tailrec
  def getParGroups(list: List[String], acc: List[String]): Either[String, (List[String], List[String])] = {
    list match {
      case "(" :: tail =>
        getParGroup(list) match {
          case Left(s) => Left(s)
          case Right((result, rest)) => getParGroups(rest, ("(" +: result :+ ")") ++ acc)
        }
      case ")" :: tail =>
        Right((acc.reverse, tail))
      case s :: tail =>
        getParGroups(tail, s :: acc)
      case Nil =>
        Left("Expected \")\"")
    }
  }


  val commands: Seq[ExecutableSubcommand] = Seq(
    new BeatEditor.Conf(),
    new editor.BeatEditor2.Conf(),
    new AudioGenerator.Conf(),
    new VideoGenerator.Conf(),
    new BPMDetector.Conf()
  )

  object Conf extends ScallopConf(args) {
    for (subcommand: ExecutableSubcommand <- commands)
      addSubcommand(subcommand)
    version("Beatmeter Generator 0.1.0")
    verify()
  }

  Conf.subcommand match {
    case Some(c: ExecutableSubcommand) =>
      c.execute(Conf.subcommands.tail, args)
    case _ => Conf.printHelp()
  }

}
