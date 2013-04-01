package com.mchange.sc.v1.pseudocase;

/**
 * Intended to be extended by Singleton objects.
 */ 
trait Pseudocase[ENC,TUP] {

  type Encoded = ENC;
  type Tupled  = TUP;

  def apply( tuple : Tupled ) : Encoded;
  def unapply( source : Encoded ) : Option[Tupled];
  def conforms( obj : Encoded ) : Boolean = (unapply( obj ) != None)

}
