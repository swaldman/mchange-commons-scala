package com.mchange.sc.v1.mappable;

import com.mchange.sc.v1.reflect._;
import com.mchange.sc.v1.util.concurrent._;
import scala.reflect.runtime.universe._;

object ClassNameExporting {
  import ReflectiveMappable.mirror;

  val StaticFactoryClassNameKey = ".staticFactoryClassName";

  def maybeFromMap( map : Map[String,Any] ) : Option[Mappable] = {

    map.get( StaticFactoryClassNameKey ) match {
      case Some( name : String ) => {
	val staticFactoryMethod = Class.forName( name ).getMethod( "fromMap", classOf[Map[String,Any]] );
	Some( staticFactoryMethod.invoke( null, map ).asInstanceOf[Mappable] );
      }
      case _ => None;
    }    
    
    /*
    map.get( ClassNameKey ) match {
      case Some( name : String ) => ReflectionInvoker.await {
	val classSymbol  : ClassSymbol  = mirror.classSymbol( Class.forName( name ) );
	val moduleSymbol : ModuleSymbol = classSymbol.companionSymbol.asModule;
	val moduleMirror : ModuleMirror = mirror.reflectModule(moduleSymbol);

	Some( moduleMirror.instance.asInstanceOf[CompanionOfMappable[Mappable]].fromMap( map ) );
      }
      case _ => None;
    }
    */ 
  }
}

trait CompanionOfClassNameExporting[T <: Mappable] extends CompanionOfMappable[T];

trait ClassNameExporting extends AnnotatedMappable {
  def staticFactoryClassName : String = this.getClass.getName;

  override def extraBindings : Iterable[ ( String, Any ) ] = Map( ClassNameExporting.StaticFactoryClassNameKey -> staticFactoryClassName );
}
