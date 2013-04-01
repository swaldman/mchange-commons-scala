package com.mchange.sc.v1.pseudocase;

import scala.reflect.runtime.universe._;
import com.mchange.sc.v1.reflect._;

import org.specs2.Specification;

object SamplePseudocaseSpec {
  val PayloadKey                = "Payload";
  val LogKey                    = "Log";
  val AuthenticationKey         = "Authentication";

  val FederationKey             = "Federation";
  val FederationMajorVersionKey = "FederationMajorVersion";
  val FederationMinorVersionKey = "FederationMinorVersion";

  val DefaultFederationName         : String = "Democognos";
  val DefaultFederationMajorVersion : Int    = 0;
  val DefaultFederationMinorVersion : Int    = 3;

  type DcMessage = Map[String,Any];
  type LogEntry  = Map[String,Any];
  type Payload   = Map[String,Any];

  object DcMessage extends Pseudocase[ Map[String,Any], Tuple3[Payload,Seq[LogEntry],Map[String,Any]] ] {

    val applyExpanded = ( 
      payload                : Payload, 
      log                    : Seq[LogEntry], 
      authentication         : Map[String,Any],  
      federation             : String,
      federationMajorVersion : Int,
      federationMinorVersion : Int
    ) => Map( 
      PayloadKey                -> payload,
      LogKey                    -> log,
      AuthenticationKey         -> authentication,
      FederationKey             -> federation,
      FederationMajorVersionKey -> federationMajorVersion,
      FederationMinorVersionKey -> federationMinorVersion
    );
    
    val applyUntupled = ( 
      payload        : Payload, 
      log            : Seq[LogEntry], 
      authentication : Map[String,Any]  
    ) => applyExpanded( payload, log, authentication, DefaultFederationName, DefaultFederationMajorVersion,  DefaultFederationMinorVersion );
    
    
    val applyTupled = applyUntupled.tupled;
    
    def apply( 
      payload                : Payload, 
      log                    : Seq[LogEntry], 
      authentication         : Map[String,Any],  
      federation             : String,
      federationMajorVersion : Int,
      federationMinorVersion : Int
    ) = applyExpanded( payload, log, authentication, federation, federationMajorVersion, federationMinorVersion );
    
    def apply(
      payload        : Payload, 
      log            : Seq[LogEntry], 
      authentication : Map[String,Any]
    ) = applyUntupled( payload, log, authentication );
    
    def apply( tuple : Tupled ) = applyTupled( tuple );
    
    def unapply( source : Encoded ) : Option[Tupled] = {
      val payload                = source.get( PayloadKey );
      
      val log                    = source.getOrElse( LogKey, Seq.empty[LogEntry] );
      val authentication         = source.getOrElse( AuthenticationKey, Map.empty[String,Any] );
      
      val federation             = source.get( FederationKey );
      val federationMajorVersion = source.get( FederationMajorVersionKey );
      val federationMinorVersion = source.get( FederationMinorVersionKey );
      
      ( payload, log, authentication, federation, federationMajorVersion, federationMinorVersion ) match {
	case ( Some( pl : Payload ), l : Seq[LogEntry], a : Map[String,Any], Some( DefaultFederationName ), Some( DefaultFederationMajorVersion ), Some( DefaultFederationMinorVersion ) ) =>  {
	  if (
	    ReflectionInvoker.await {
	    // apparently this style of type reflection doesn't work for aliased types. hmmm...
 
	    //instanceToCompileTimeType(pl) <:< typeOf[Payload] && 
	    //instanceToCompileTimeType(l)  <:< typeOf[Seq[LogEntry]] && 
	    instanceToCompileTimeType(a)  <:< typeOf[Map[String,Any]] 
	    }
	  ) // do what the compiler can't yet due to erasure
	    Some( Tuple3(pl, l, a) );
	  else
	    None;
	}
	case _ => None;
      }
    }
    
    def payload( message : DcMessage ) : Any = message match {
      case DcMessage( payload, _, _ ) => payload;
    }
    
    def log( message : DcMessage ) : Seq[LogEntry] = message match {
      case DcMessage( _, log, _ ) => log;
    }
    
    def authentication( message : DcMessage ) : Map[String,Any] = message match {
      case DcMessage( _, _, auth ) => auth;
    }
  }

  val testPayload   = Map("PayloadKey"->999);
  val testLog       = Seq.empty[LogEntry];
  val emptyAuth     = Map.empty[String,Any];

  val testDcMessage = DcMessage( testPayload, testLog, emptyAuth );

  val expectedMap = Map( 
    PayloadKey -> testPayload,
    LogKey -> testLog,
    AuthenticationKey -> emptyAuth,
    FederationKey -> DefaultFederationName,
    FederationMajorVersionKey -> DefaultFederationMajorVersion,
    FederationMinorVersionKey -> DefaultFederationMinorVersion
  )
  
}

class SamplePseudocaseSpec extends Specification {
  import SamplePseudocaseSpec._;

  def is = {
    "Of our sample Pseudocase (an abortive version of DcMessage)"                               ^
    "its extracted payload must equal the payload with which it was constructed" ! checkPayload ^
    "its extracted log must equal the log with which it was constructed"         ! checkLog     ^
    "its extracted auth must equal the auth with which it was constructed"       ! checkAuth    ^
    "it must equal its expected form constructed as a simple Map"                ! verifyMap
  }

  def checkPayload = DcMessage.payload( testDcMessage ) mustEqual testPayload;
  def checkLog     = DcMessage.log( testDcMessage ) mustEqual testLog;
  def checkAuth    = DcMessage.authentication( testDcMessage ) mustEqual emptyAuth;
  def verifyMap    = testDcMessage mustEqual expectedMap;
}
