package utils

import java.sql.{Connection, Array, PreparedStatement}

import anorm.{MetaDataItem, Column, ToStatement, TypeDoesNotMatch}
import org.postgresql.util.PGobject

/**
 * Created by sharon on 14-12-25.
 */
object AnormHelper {
  case class Location(lat: Double, long: Double)

  /**
   * Converts uncommon postgresql sql types (PGobject) to String.
   *
   * @return a string instance of the postgresql sql value
   */
  implicit def columnToString: Column[String] = Column.nonNull { (value, _) =>
    value match {
      case pgo: PGobject =>
        pgo.getType match {
          case "citext" => Right(pgo.getValue)
          case x => Left(TypeDoesNotMatch(s"Cannot convert sql type ${pgo.getType} to ${x.getClass.toString}"))
        }
      case s: java.lang.String => Right(s)
      case x => Left(TypeDoesNotMatch(s"Conversion to ${x.getClass.toString} was not defined"))
    }
  }


//  implicit def columnToChar: Column[Char] = Column.nonNull { (value, _) =>
//    value match {
//      case pgo: PGobject =>
//        pgo.getType match {
//          case "citext" => Right(pgo.getValue)
//          case x => Left(TypeDoesNotMatch(s"Cannot convert sql type ${pgo.getType} to ${x.getClass.toString}"))
//        }
//      case x => Left(TypeDoesNotMatch(s"Conversion to ${x.getClass.toString} was not defined"))
//    }
//  }

  implicit object locationToStatement extends ToStatement[Location] {
    def set(s: PreparedStatement, i: Int, l: Location) = s.setObject(i, s"POINT(${l.lat} ${l.long})")
  }

  implicit object arrayToStatement extends ToStatement[Array] {
    def set(s: PreparedStatement, i: Int, a: Array) = s.setArray(i, a)
  }

  def toSqlArray(l: List[String])(implicit c: Connection): Array = {
    c.createArrayOf("varchar", l.toArray)
  }
}
