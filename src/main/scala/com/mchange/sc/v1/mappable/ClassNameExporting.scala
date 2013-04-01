package com.mchange.sc.v1.mappable;

import com.mchange.sc.v1.reflect._;
import com.mchange.sc.v1.util.concurrent._;
import scala.reflect.runtime.universe._;

object ClassNameExporting {
  import ReflectiveMappable.mirror;

  val ClassNameKey = ".className";

  def maybeFromMap( map : Map[String,Any] ) : Option[Mappable] = {
    map.get( ClassNameKey ) match {
      case Some( name : String ) => ReflectionInvoker.await {
	val classSymbol  : ClassSymbol  = mirror.classSymbol( Class.forName( name ) );
	val moduleSymbol : ModuleSymbol = classSymbol.companionSymbol.asModule;
	val moduleMirror : ModuleMirror = mirror.reflectModule(moduleSymbol);

	Some( moduleMirror.instance.asInstanceOf[CompanionOfMappable[Mappable]].fromMap( map ) );
      }
      case _ => None;
    }
  }
}

trait ClassNameExporting extends ReflectiveMappable {
  override def extraBindings : Iterable[ ( String, Any ) ] = Map( ClassNameExporting.ClassNameKey -> this.getClass.getName );
}
