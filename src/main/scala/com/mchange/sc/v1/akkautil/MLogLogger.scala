package com.mchange.sc.v1.akkautil;

import akka.actor._;
import akka.event.Logging._;
import akka.event.DummyClassForStringSources;

import com.mchange.sc.v1.log._;

object MLogLogger {
  implicit lazy val log = MLogger( this );
}

// adapted from akka.event.slf4j.Slf4jLogger
class MLogLogger extends Actor {

  private def logFormat( logger : MLogger, level : MLevel, message : Any ) : Unit = {
    stringFormat( message ) match {
      case string : String => logger.log( level, string );
      case array : Array[_] => logger.logFormat( level, "{}", array ); // note this would be one-element array made by stringFormat
      case other =>  {
	throw new InternalError( s"Unexpected result from StringMessage, orig message: '${message}', transformed: '${other}'" );
      }
    }
  }

  /* returns String or Array only */ 
  private def stringFormat( message : Any ) : AnyRef = message match {
    case string : String => string;
    case array : Array[_] => array.mkString("[",", ","]");
    case seq : Seq[_] => seq.mkString("[",", ","]");
    case null => "null";
    case other => Array(message);
  }

  private def logger( logClass : Class[_],  logSource : String ) : MLogger = {
    if ( logClass == classOf[DummyClassForStringSources] )
      MLogger( logSource );
    else
      MLogger( logClass );
  }

  def receive = {

    case event @ Error(cause, logSource, logClass, message) => cause match {
      case Error.NoCause | null => {
	if ( message == null )
	  logger(logClass, logSource).log(MLevel.SEVERE, event.toString + " <Unknown Cause>")
	else
	  logFormat( logger(logClass, logSource), MLevel.SEVERE, message );
      }
      case cause => {
	if (message == null)
	  logger(logClass, logSource).log(MLevel.SEVERE, cause.getLocalizedMessage(), cause);
	else {
	  stringFormat( message ) match {
	    case string : String => logger(logClass, logSource).log(MLevel.SEVERE, string, cause);
	    case array : Array[_] => logger(logClass, logSource).log( MLevel.SEVERE, java.text.MessageFormat.format("{}", array ), cause );
	    case other => {
	      throw new InternalError( s"Unexpected result from StringMessage, orig message: '${message}', transformed: '${other}'" );
	    }
	  }
	}
      }
    }
    case event @ Warning(logSource, logClass, message) => logFormat( logger(logClass, logSource), MLevel.WARNING, message );
    case event @ Info(logSource, logClass, message) => logFormat( logger(logClass, logSource), MLevel.INFO, message );
    case event @ Debug(logSource, logClass, message) => logFormat( logger(logClass, logSource), MLevel.FINER, message );

    case InitializeLogger(_) ⇒
      MLogLogger.log.info("MLogLogger started")
      sender ! LoggerInitialized
  }
}
