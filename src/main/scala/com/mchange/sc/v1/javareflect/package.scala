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

package com.mchange.sc.v1;

import language.implicitConversions;

import com.mchange.v2.log._;

package object javareflect {

  implicit lazy val logger : MLogger = MLog.getLogger( packageNameForPackageObject( this ) );

  def toClass( fqcn : String ) : Option[Class[_ <: AnyRef]] = try {
    Some( Class.forName( fqcn ).asInstanceOf[Class[_ <: AnyRef]] );
  } catch {
    case e : Exception => {
      logger.log( MLevel.WARNING, "Could not find class for fully qualified class name '%s'.".format( fqcn ), e );
      None;
    }
  }

  implicit def stringToPimpedMaybeClass( fqcn : String ) : PimpedMaybeClass = new PimpedMaybeClass( toClass( fqcn ) );

  def newInstance( clz : Class[_ <: AnyRef], argTypes : Seq[Class[_]] = Nil, args : Seq[AnyRef] = Nil ) : Option[AnyRef] = try {
    if (argTypes.length == 0)
      Some( clz.newInstance() );
    else {
      Some( clz.getConstructor( argTypes : _* ).newInstance( args.toArray[java.lang.Object] ) );
    }
  } catch {
    case e : Exception => {
      logger.log( MLevel.WARNING, "Could not instantiate class %s with argTypes [%s] and args [%s] supplied.".format( clz.getName(), argTypes, args ), e );
      None;
    }
  }

  def javaForwarderFqcn( obj : Singleton ) : String = {
    var raw = obj.getClass.getName;
      if ( raw.endsWith( "$" ) )
	raw.substring(0, raw.length() - 1);
      else
	raw;
  }

  def packageNameForPackageObject( packageObject : Singleton ) : String = {
    val name = packageObject.getClass().getName();
    name.substring(0, name.lastIndexOf('.'));
  }
}

