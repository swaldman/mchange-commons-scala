/*
 * Distributed as part of mchange-commons-scala
 *
 * Copyright (C) 2013 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1, as 
 * published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this software; see the file LICENSE.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

package com.mchange.sc.v1.sql;

import java.sql.{Connection,DriverManager,PreparedStatement,Statement,ResultSet};
import javax.sql.DataSource;
import com.mchange.sc.v1.util.ClosableUtils._;

object ResourceUtils
{
  type ConnectionSource = { def getConnection() : Connection } // javax.sql.DataSource conforms, but when we don't need a full implementation

  def connectionSource( jdbcUrl : String, jdbcUserName : String, jdbcPassword : String ) : ConnectionSource =
  {
    if ( jdbcUserName == null )
      new { def getConnection() = DriverManager.getConnection( jdbcUrl ); }
    else
      new { def getConnection() = DriverManager.getConnection( jdbcUrl, jdbcUserName, jdbcPassword ); }
  }

  def connectionSource( jdbcUrl : String ) : ConnectionSource = connectionSource( jdbcUrl, null, null );

  def withConnection[T]( csrc : ConnectionSource )( op : (Connection) => T) : T =
  {
    var con : Connection = null;
    
    try
    {
      con = csrc.getConnection();
      op(con);
    }
    finally
    { attemptClose( con ); }
  }

  // to avoid the reflective method call when we have a real javax.sql.DataSource
  def withConnection[T]( csrc : DataSource )( op : (Connection) => T) : T =
  {
    var con : Connection = null;
    
    try
    {
      con = csrc.getConnection();
      op(con);
    }
    finally
    { attemptClose( con ); }
  }

  def withStatement[T]( con : Connection )( op : (Statement) => T) : T =
  {
    var stmt : Statement = null;
    
    try
    {
      stmt = con.createStatement();
      op(stmt);
    }
    finally
    { attemptClose( stmt ); }
  }

  def withPreparedStatement[T]( con : Connection, sql : String )( op : (PreparedStatement) => T ) : T =
  {
    var ps : PreparedStatement = null;
    
    try
    {
      ps = con.prepareStatement( sql );
      op( ps );
    }
    finally
    { attemptClose( ps ); }
  }

  def withResultSet[T]( stmt : Statement, query : String )( op : (ResultSet) => T ) : T =
  {
    var rs : ResultSet = null;

    try
    {
      rs = stmt.executeQuery( query );
      op( rs );
    }
    finally
    { attemptClose( rs ); }
  }

  def withResultSet[T]( ps : PreparedStatement )( op : (ResultSet) => T ) : T =
  {
    var rs : ResultSet = null;

    try
    {
      rs = ps.executeQuery();
      op( rs );
    }
    finally
    { attemptClose( rs ); }
  }

  def withWrappedPreparedStatement[T]( con : Connection, mgr : PsManager)( op : (WrappedPreparedStatement) => T ) : T =
  {
    var wps : WrappedPreparedStatement = null;

    try
    {
      wps = mgr.prepareStatement(con);
      op( wps );
    }
    finally
    { attemptClose( wps ); }
  }
}

