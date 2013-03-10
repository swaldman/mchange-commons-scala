package com.mchange.sc.v1.log;

import java.util.ResourceBundle;

object MLogger {
  def apply( name : String ) : MLogger = {
    val unwrapped = com.mchange.v2.log.MLog.getLogger( name );
    new MLogger( unwrapped );
  }

  def apply( obj : Any ) : MLogger = apply ( obj.getClass.getName );
}

class MLogger( private[log] val inner : com.mchange.v2.log.MLogger ){

  import MLevel._;

  implicit val logger : MLogger = this;

  def resourceBundle : ResourceBundle = inner.getResourceBundle();
  def resourceBundleName : String = inner.getResourceBundleName();
   def level_=( level : MLevel) : Unit = inner.setLevel( level._level );
  def level : MLevel = MLevel.forInner( inner.getLevel() );
  
  def log( level : MLevel, message : =>String ) : Unit = level.doIf( inner.log( _, message ) );
  def log( level : MLevel, message : =>String, error : =>Throwable ) : Unit = level.doIf( inner.log( _, message, error ) );
  def logFormat( level : MLevel, message : =>String, params : =>Seq[Any] ) = level.doIf( inner.log( _, message, params.toArray ) );
  def logp( level : MLevel, sourceClass : =>String, sourceMethod : =>String,  message : =>String) = level.doIf( inner.logp( _, sourceClass, sourceMethod, message) );
  def logp( level : MLevel, sourceClass : =>String, sourceMethod : =>String,  message : =>String, error : =>Throwable) = level.doIf( inner.logp( _, sourceClass, sourceMethod, message, error) );
  def logpFormat( level : MLevel, sourceClass : =>String, sourceMethod : =>String,  message : =>String, params : =>Seq[Any] ) = level.doIf( inner.logp( _, sourceClass, sourceMethod, message, params.toArray) );
  def logrb( level : MLevel, sourceClass : =>String, sourceMethod : =>String, resourceBundle : =>String, message : =>String) = level.doIf( inner.logrb( _, sourceClass, sourceMethod, resourceBundle, message) );
  def logrb( level : MLevel, sourceClass : =>String, sourceMethod : =>String, resourceBundle : =>String, message : =>String, error : =>Throwable) = level.doIf( inner.logrb( _, sourceClass, sourceMethod, resourceBundle, message, error) );
  def logrbFormat( level : MLevel, sourceClass : =>String, sourceMethod : =>String, resourceBundle : =>String, message : =>String, params : =>Seq[Any] ) = level.doIf( inner.logrb( _, sourceClass, sourceMethod, resourceBundle, message, params.toArray) );
  
  def entering(sourceClass : =>String, sourceMethod : =>String) = FINER.doIf( l => inner.entering( sourceClass, sourceMethod ) );
  def entering(sourceClass : =>String, sourceMethod : =>String, params : =>Seq[Any] ) = FINER.doIf( l => inner.entering( sourceClass, sourceMethod, params ) );
  def exiting(sourceClass : =>String, sourceMethod : =>String) = FINER.doIf( l => inner.exiting( sourceClass, sourceMethod ) );
  def exiting(sourceClass : =>String, sourceMethod : =>String, params : =>Seq[Any] ) = FINER.doIf( l => inner.exiting( sourceClass, sourceMethod, params ) );
  def throwing(sourceClass : String, sourceMethod : String, error : Throwable) = FINER.doIf( l => inner.throwing( sourceClass, sourceMethod, error ) );
  
  
  private def levelMessage( level : MLevel, message : =>String ) = level.doIf( l => inner.log(l, message ) );
  
  def severe(message : =>String) = levelMessage(SEVERE, message);
  def warning(message : =>String) = levelMessage(WARNING, message);
  def info(message : =>String) = levelMessage(INFO, message);
  def config(message : =>String) = levelMessage(CONFIG, message);
  def fine(message : =>String) = levelMessage(FINE, message);
  def finer(message : =>String) = levelMessage(FINER, message);
  def finest(message : =>String) = levelMessage(FINEST, message);
}
