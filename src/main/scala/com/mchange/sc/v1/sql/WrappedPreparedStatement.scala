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

import java.sql.{PreparedStatement,ResultSet};

object WrappedPreparedStatement
{
  implicit def unwrap( wps : WrappedPreparedStatement ) : PreparedStatement = wps.toPreparedStatement;
}

class WrappedPreparedStatement( mgr : PsManager, ps : PreparedStatement )
{
  def paramNames = mgr.paramNames;

  def nullParameters() = mgr.nullParameters( ps );

  def accept(params : Map[String,Any]) = mgr.overwriteParameters( ps, params );
  
  def setParameter( i : Int, param : Any ) = mgr.setParameter( ps, i, param );
  
  def setParameter( paramName : String, param : Any ) = mgr.setParameter( ps, paramName, param );

  def toPreparedStatement() = ps;

  def <<(params : Map[String,Any]) : WrappedPreparedStatement = { this.accept(params); this; }

  def <<(param : Pair[String,Any]) : WrappedPreparedStatement = { this.setParameter( param._1, param._2 ); this }

  def <<(params : Pair[String,Any]*) : WrappedPreparedStatement = { this.accept( Map( params : _* ) ); this }

  def !! : Int = ps.executeUpdate();

  def ?? : ResultSet = ps.executeQuery();

  def ~! : Unit = ps.addBatch()

  def ~!! : Array[Int] = ps.executeBatch();
}
