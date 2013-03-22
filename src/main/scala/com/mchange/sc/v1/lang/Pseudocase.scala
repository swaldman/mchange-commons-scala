package com.mchange.v1.lang;

trait Pseudocase[ENC,TUP] {
  type > = ENC;

  def apply( tuple : TUP ) : ENC;
  def unapply( source : ENC ) : Option[TUP];
  def conforms( obj : ENC ) : Boolean = (unapply( obj ) != None)
}
