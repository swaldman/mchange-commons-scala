package com.mchange.sc.v1.log;

object MLevel {
  private[log] def forInner( inner : com.mchange.v2.log.MLevel ) : MLevel = inner match {
    case com.mchange.v2.log.MLevel.ALL => ALL
    case com.mchange.v2.log.MLevel.CONFIG => CONFIG
    case com.mchange.v2.log.MLevel.FINE => FINE
    case com.mchange.v2.log.MLevel.FINER => FINER
    case com.mchange.v2.log.MLevel.FINEST => FINEST
    case com.mchange.v2.log.MLevel.INFO => INFO
    case com.mchange.v2.log.MLevel.OFF => OFF
    case com.mchange.v2.log.MLevel.SEVERE => SEVERE
    case com.mchange.v2.log.MLevel.WARNING => WARNING
  }

  case object ALL extends MLevel( com.mchange.v2.log.MLevel.ALL );
  case object CONFIG extends MLevel( com.mchange.v2.log.MLevel.CONFIG );
  case object FINE extends MLevel( com.mchange.v2.log.MLevel.FINE );
  case object FINER extends MLevel( com.mchange.v2.log.MLevel.FINER );
  case object FINEST extends MLevel( com.mchange.v2.log.MLevel.FINEST );
  case object INFO extends MLevel( com.mchange.v2.log.MLevel.INFO );
  case object OFF extends MLevel( com.mchange.v2.log.MLevel.OFF );
  case object SEVERE extends MLevel( com.mchange.v2.log.MLevel.SEVERE );
  case object WARNING extends MLevel( com.mchange.v2.log.MLevel.WARNING );
}

sealed abstract class MLevel ( private[log] val _level : com.mchange.v2.log.MLevel ) {
  private[log] def doIf[T]( op : com.mchange.v2.log.MLevel => T )(implicit logger : MLogger) : Unit = if ( logger.inner.isLoggable( _level ) ) op( _level )

  def log( message : =>String )( implicit logger : MLogger ) = doIf( logger.inner.log( _, message ) );
  def log( message : =>String, error : =>Throwable )( implicit logger : MLogger ) = doIf( logger.inner.log( _, message, error ) );
  def logFormat( message : =>String, params : Seq[Any] )( implicit logger : MLogger ) = doIf( logger.inner.log( _, message, params.toArray ) );
}



