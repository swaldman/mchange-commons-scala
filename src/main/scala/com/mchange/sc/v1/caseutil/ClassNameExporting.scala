package com.mchange.sc.v1.caseutil;

import com.mchange.sc.v1.reflect._;
import com.mchange.sc.v1.util.concurrent._;
import scala.reflect.runtime.universe._;

object ClassNameExporting {
  import ReflectiveValMappedCase.mirror;

  val ClassNameKey = ".className";

  def fromMap( map : Map[String,Any] ) : Option[ValMappedCase] = {
    map.get( ClassNameKey ) match {
      case Some( name : String ) => ReflectionInvoker.await {
	val classSymbol  : ClassSymbol  = mirror.classSymbol( Class.forName( name ) );
	val moduleSymbol : ModuleSymbol = classSymbol.companionSymbol.asModule;
	val moduleMirror : ModuleMirror = mirror.reflectModule(moduleSymbol);

	Some( moduleMirror.instance.asInstanceOf[CompanionOfValMappedCase[ValMappedCase]].fromMap( map ) );
      }
      case _ => None;
    }
  }
}

trait ClassNameExporting extends ReflectiveValMappedCase {
  override def extraBindings : Iterable[ ( String, Any ) ] = Map( ClassNameExporting.ClassNameKey -> this.getClass.getName );
}
