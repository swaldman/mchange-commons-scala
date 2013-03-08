package com.mchange.sc.v1.javareflect;

class PimpedMaybeClass(val maybeClass : Option[Class[_ <: AnyRef]]) {
  def newInstance( argTypes : Seq[Class[_]] = Nil, args : Seq[AnyRef] = Nil ) : Option[AnyRef] = {
    if (maybeClass == None)
      None;
    else
      com.mchange.sc.v1.javareflect.newInstance( maybeClass.get, argTypes, args );
  }
}
