package com.mchange.sc.v2

import com.mchange.sc.v2.lang.borrow

import java.sql.{Connection,PreparedStatement,ResultSet}

import scala.util.control.NonFatal

package object sql {

  final class UnexpectedRowCountException( message : String ) extends Exception( message )

  def borrowTransact[T]( connectionSource : =>Connection )( block : Connection => T ) : T = borrow( connectionSource )( conn => transact( conn )( block ) )

  def transact[T]( conn : Connection )( block : Connection => T ) : T = {
    val origAutoCommit = conn.getAutoCommit()

    try {
      conn.setAutoCommit(false)
      val out = block( conn )
      conn.commit()
      out
    } catch {
      case NonFatal( t ) => {
        conn.rollback()
        throw t
      }
    } finally {
      conn.setAutoCommit( origAutoCommit )
    }
  }

  def getSingleValue[T]( extractor : ResultSet => T)( rs : ResultSet ) : T = {
    if (! rs.next() ) throw new UnexpectedRowCountException("A result set expected to contain precisely one row contained no rows at all")
    val out = extractor( rs )
    if ( rs.next() ) throw new UnexpectedRowCountException("A result set expect to contain precisely one row contained more than one row")
    out
  }

  def getSingleString( rs : ResultSet )  = getSingleValue( _.getString(1) )( rs )
  def getSingleBoolean( rs : ResultSet ) = getSingleValue( _.getBoolean(1) )( rs )
  def getSingleInt( rs : ResultSet )     = getSingleValue( _.getInt(1) )( rs )
  def getSingleLong( rs : ResultSet )    = getSingleValue( _.getLong(1) )( rs )
  def getSingleFloat( rs : ResultSet )   = getSingleValue( _.getFloat(1) )( rs )
  def getSingleDouble( rs : ResultSet )  = getSingleValue( _.getDouble(1) )( rs )

  def getMaybeSingleValue[T]( extractor : ResultSet => T)( rs : ResultSet ) : Option[T] = {
    if (! rs.next() ) {
      None
    } else {
      val out = extractor( rs )
      if ( rs.next() ) throw new UnexpectedRowCountException("A result set expect to contain precisely one row contained more than one row")
      Some( out )
    }
  }

  def getMaybeSingleString( rs : ResultSet )  = getMaybeSingleValue( _.getString(1) )( rs )
  def getMaybeSingleBoolean( rs : ResultSet ) = getMaybeSingleValue( _.getBoolean(1) )( rs )
  def getMaybeSingleInt( rs : ResultSet )     = getMaybeSingleValue( _.getInt(1) )( rs )
  def getMaybeSingleLong( rs : ResultSet )    = getMaybeSingleValue( _.getLong(1) )( rs )
  def getMaybeSingleFloat( rs : ResultSet )   = getMaybeSingleValue( _.getFloat(1) )( rs )
  def getMaybeSingleDouble( rs : ResultSet )  = getMaybeSingleValue( _.getDouble(1) )( rs )

  def setMaybeString( sqlType : Int )( ps : PreparedStatement, index : Int, mbValue : Option[String] )  : Unit = {
    mbValue.fold( ps.setNull( index, sqlType ) )( value => ps.setString( index, value ) )
  }
}
