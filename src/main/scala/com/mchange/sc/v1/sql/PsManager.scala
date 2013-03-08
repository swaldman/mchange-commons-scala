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

import java.io.{BufferedInputStream,BufferedReader,ByteArrayInputStream,ByteArrayOutputStream,InputStream,Reader,StringReader};
import java.sql.{Blob,Clob,Connection,PreparedStatement,Ref,Time,Timestamp};
import java.sql.Types._;
import com.mchange.sc.v1.util.ClosableUtils._;
import scala.collection._;


object PsManager
{
  val BUFFER_SIZE = 16 * 1024 * 1024;

  def setParameter(ps       : PreparedStatement, 
		   i        : Int, 
		   value    : Any, 
		   typeCode : Int) : Unit =
		     setParameter( ps, i, value, typeCode, None );
		     

  def setParameter(ps       : PreparedStatement, 
		   i        : Int, 
		   value    : Any, 
		   typeCode : Int,
		   length   : Option[Long]) : Unit =
  {
    value match
    {
      case ( null ) => ps.setNull( i, typeCode );
      case ( bool : Boolean ) => ps.setBoolean( i, bool );
      case ( byte : Byte ) => ps.setByte( i, byte );
      case ( bs : Array[Byte] ) => ps.setBytes( i, bs );
      case ( s : Short ) => ps.setShort( i, s );
      case ( num : Int ) => ps.setInt( i, num );
      case ( f : Float ) => ps.setFloat( i, f );
      case ( d : Double ) => ps.setDouble( i, d );
      case ( arr : java.sql.Array ) => ps.setArray( i, arr );
      case ( sbd : BigDecimal ) => ps.setBigDecimal( i, sbd.bigDecimal );
      case ( jbd : java.math.BigDecimal ) => ps.setBigDecimal( i, jbd );
      case ( blob : Blob ) => ps.setBlob( i, blob );
      case ( clob : Clob ) => ps.setClob( i, clob );
      case ( sdate : java.sql.Date) => ps.setDate( i, sdate );
      case ( stime : Time ) => ps.setTime( i, stime );
      case ( stimestamp : Timestamp ) => ps.setTimestamp( i, stimestamp );
      case ( date : java.util.Date ) => 
	{
	  typeCode match
	  {
	    case DATE => ps.setDate( i, new java.sql.Date(date.getTime()) );
	    case TIME => ps.setTime( i, new java.sql.Time(date.getTime()) );
	    case _    => ps.setTimestamp( i, new java.sql.Timestamp(date.getTime()) ); // the default is TIMESTAMP
	  }
	}
      case ( ref : Ref ) => ps.setRef( i, ref );
      case ( str : String ) => ps.setString( i, str );
      case ( reader : Reader ) =>
	{
	  if ( length == None )
	    {
	      val sb = new StringBuilder( BUFFER_SIZE );

	      var br : BufferedReader = null;

	      try
	      {
		br = new BufferedReader( reader, BUFFER_SIZE );

		var c : Int = br.read();
		while ( c >= 0 )
		{
		  sb.append( c.toChar );
		  c = br.read();
		}

		val str = sb.toString();
		setParameter( ps, i, new StringReader( str ), typeCode, Some( str.length ) );
	      }
	      finally 
	      { attemptClose( br ); }
	    }
	  else
	    ps.setCharacterStream( i, reader, length.get );
	}
      case ( is : InputStream ) =>
	{
	  if ( length == None )
	    {
	      val baos = new ByteArrayOutputStream( BUFFER_SIZE );

	      var bis : BufferedInputStream = null;

	      try
	      {
		bis = new BufferedInputStream( is, BUFFER_SIZE );

		var b : Int = bis.read();
		while ( b >= 0 )
		{
		  baos.write(b)
		  b = bis.read();
		}

		val ba = baos.toByteArray;
		setParameter( ps, i, new ByteArrayInputStream( ba ), typeCode, Some( ba.length ) );
	      }
	      finally 
	      { attemptClose( bis ); }
	    }
	  else
	    ps.setBinaryStream( i, is, length.get );
	}
      case (any : AnyRef) =>
	{
	  if ( length == None )
	    ps.setObject( i, any, typeCode );
	  else
	    ps.setObject( i, any, typeCode, length.get.toInt );
	}
      case whatever @ _ => throw new IllegalArgumentException("Don't know what to do with parameter: " + whatever );
    }
  }

  def apply( sql : String, paramNames : List[String], paramTypeCodes : List[Int] ) = new PsManager( sql, paramNames, paramTypeCodes );

  // sql here refers to the package name (interesting)
  private[sql] def zipInfoLists(paramNames : List[String], paramTypeCodes : List[Int]) =
    {
      require( paramNames.length == paramTypeCodes.length );
      paramNames.zip( paramTypeCodes );
    }
}

/**
 *  info is a list of Tuples including the name of the column
 *  being substituted and the java.sql.Types typecode associated
 *  with the column.
 */ 
class PsManager( val sql : String, info : RandomAccessSeq[Tuple2[String,Int]] )
{
  // we work with info directly only in the following 5 functions,
  // 'cuz jumping between 1-based and zero-based is bug-prone
  // and annoying. we get these right, and do everything else
  // in terms of them.
  val indexForName : Map[String,Int] = { immutable.Map.empty[String,Int] ++ ( for (i <- 0 until info.length) yield ( info(i)._1 -> (i+1) ) ); }
  def tupForIndex( i : Int ) = info(i - 1);
  def nameForIndex( i : Int ) = tupForIndex(i)._1;
  def paramNames = info.map( _._1 );
  def tupForName( name : String ) = info( indexForName(name) - 1);
  val numParameters = info.length;


  def prepareStatement(con : Connection) : WrappedPreparedStatement = wrap( con.prepareStatement( sql ) );

  def apply(con : Connection) = prepareStatement(con);

  def setParameter( ps : PreparedStatement, i : Int, param : Any ) = 
    {
      val strTypeTup = tupForIndex(i);
      PsManager.setParameter(ps, i, param, strTypeTup._2);
    }

  def setParameter( ps : PreparedStatement, paramName : String, param : Any ) = 
    {
      val i = indexForName( paramName ); //throws an Exception if paramName is not found
      val strTypTup = tupForIndex(i);
      PsManager.setParameter(ps, i, param, strTypTup._2);
    }

  def overwriteParameters( ps : PreparedStatement, params : Map[String,Any] ) = params.foreach( tup => setParameter( ps, tup._1, tup._2 ) );

  def nullParameters( ps : PreparedStatement ) = for (i <- 1 to numParameters) setParameter( ps, i, null );

  def wrap( ps : PreparedStatement ) = new WrappedPreparedStatement( this, ps );

  def this( sql : String, paramInfos : List[Tuple2[String,Int]] ) = this( sql, RandomAccessSeq( paramInfos : _*) );

  def this( sql : String, paramNames : List[String], paramTypeCodes : List[Int] ) = this( sql, PsManager.zipInfoLists( paramNames, paramTypeCodes ) );
}

