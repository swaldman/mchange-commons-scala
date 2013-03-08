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
