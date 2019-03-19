/*
 * Distributed as part of mchange-commons-scala v0.4.9
 *
 * Copyright (C) 2019 Machinery For Change, LLC
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php
 *
 */

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
