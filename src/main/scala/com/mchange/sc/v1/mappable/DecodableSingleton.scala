package com.mchange.sc.v1.mappable;

import com.mchange.sc.v1.decode._;
import com.mchange.sc.v1.javareflect._;


trait DecodableSingleton extends Decodable with CompanionOfDecodable[DecodableSingleton] {
  override def _toMap : Map[String,Any]                               = Map.empty[String,Any];
  override def fromMap( map : Map[String,Any] ) : DecodableSingleton  = this;
  override def staticFactoryClassName : String                        = javaForwarderFqcn( this );
}
