package com.mchange.sc.v1;

import com.mchange.v2.log._;

package object javareflect {

  val logger : MLogger = MLog.getLogger( packageNameForPackageObject( this ) );

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

