package com.mchange.sc.v1.lang;

import com.mchange.sc.v1.reflect._;
import com.mchange.sc.v1.util.concurrent._;
import scala.reflect.runtime.universe._;

import java.util.concurrent.Callable;

object ValMappedCase {
  val mirror = runtimeMirror( this.getClass.getClassLoader );
}

/**
 * T should be the type of the case class.
 *
 * tType should be the type of that class.
 */ 
trait CompanionOfValMappedCase[T] {
  import ValMappedCase.mirror;

  def tType : Type;

  private lazy val tuple = {

    val callable = new Callable[ Tuple2[ List[List[Symbol]], MethodMirror ] ] {

      def call() : Tuple2[ List[List[Symbol]], MethodMirror ] = {

	println( "Making tuple..." );
    
	val _tType   : Type         = tType;
	val ctorDecl : Symbol       = _tType.declaration(nme.CONSTRUCTOR);
	val ctor     : MethodSymbol = ctorDecl.asMethod;
	
	val ctorParamss : List[List[Symbol]] = ctor.paramss;
	
	val tClass       : ClassSymbol    = _tType.typeSymbol.asClass;
	val tClassMirror : ClassMirror    = mirror.reflectClass( tClass );
	val ctorF        : MethodMirror   = tClassMirror.reflectConstructor( ctor );  
	
	( ctorParamss, ctorF )
      }
    }

    ReflectionInvoker.invokeAndWait( callable )
  }

  private lazy val ctorParamss = tuple._1;
  private lazy val ctorF = tuple._2;

  def fromMap( map : Map[String,Any] ) : T  = {

    val callable = new Callable[T]() {
      def call() : T = {
	if (ctorParamss == Nil)
	  ctorF().asInstanceOf[T];
	else {
	  val argListTermSymbols = ctorParamss.head;
	  ctorF( argListTermSymbols.map( _.name.decoded.trim ).map( str => map(str) ) : _* ).asInstanceOf[T];
	}
      }
    }
   ReflectionInvoker.invokeAndWait( callable  );
  }
}

trait ValMappedCase {
  import ValMappedCase.mirror;

  def tType : scala.reflect.runtime.universe.Type;

  lazy val toMap : Map[String,Any] = {

    val callable = new Callable[Map[String,Any]] {

      def call() : Map[String,Any] = {
	/*
	 // fragile, breaks when Scala thinks companion is an inner type, which
	 // seems to happen even when it doesn't look like an inner type.
	 
	 // see http://stackoverflow.com/questions/11020746/get-companion-object-instance-with-new-scala-reflection-api
	 // see https://gist.github.com/xeno-by/4985929
	 // http://stackoverflow.com/questions/11084408/scala-reflection-error-this-is-an-inner-module-use-reflectmodule-on-an-instanc
	 
	 val classSymbol  : ClassSymbol  = mirror.classSymbol( this.getClass );
	 val moduleSymbol : ModuleSymbol = classSymbol.companionSymbol.asModule;
	 val moduleMirror : ModuleMirror = mirror.reflectModule(moduleSymbol);
	 
	 val tType = moduleMirror.instance.asInstanceOf[CompanionOfValMappedCase[_]].tType;
	 */
      
	val myReflection = mirror.reflect( ValMappedCase.this );
	val valSymbols   = tType.members.filter( _.isTerm ).map( _.asTerm ).filter( _.isVal );
	val valBindings  = valSymbols.map( sym => ( sym.name.decoded.trim, myReflection.reflectField( sym ).get ) );
	Map( valBindings.toSeq : _* )
      }

    }

    /*
     // was verbose for debugging

    val valBindings  = valSymbols.map( sym => symbolToBinding( myReflection, sym ) );

    private def symbolToBinding( myReflection : InstanceMirror, sym : TermSymbol ) = {
    val name = sym.name.decoded.trim;
    val value = myReflection.reflectField( sym ).get;
    ( name, value )
   */ 

   ReflectionInvoker.invokeAndWait( callable  );
  }
}


