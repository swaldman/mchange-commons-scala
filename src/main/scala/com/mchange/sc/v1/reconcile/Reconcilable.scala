/*
 * Distributed as part of mchange-commons-scala v0.4.9
 *
 * Copyright (C) 2019 Machinery For Change, LLC
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php
 *
 */

package com.mchange.sc.v1.reconcile;

/**
 * The idea here is to define a kind of value object that may be specified
 * or acquired in a manner that is only partially complete. Information from multiple instances
 * of such Objects may be "reconciled" to yield a more complete description of the
 * business object being modeled.
 *
 * Data that may sometimes be unspecified should be modeled as Option[_] objects.
 *
 * Reconciliation can be symmetrical, such that clearly inconsistent data trigger
 * a CantReconcileException, or may be asymmetrical, such that data in higher priority
 * objects shadow conflicting data in lower priority objects. reconcile and
 * reconcileLeaf are symmetrical implementations, reconcileOver and reconcileOverLeaf
 * are asymmetrical implementations.
 * 
 * Reconcilable classes should generally be final.
 * Utility methods only try to reconcile objects of identical implementation class.
 */
abstract trait Reconcilable[T <: Reconcilable[T]]
{
  def reconcile(other : T) : T;
  def reconcileOver(other : T) : T;
}

object Reconcilable
{
  /**
   * For when one is retrieving values from multiple
   * sources and any unique non-None value wins.
   */
  def reconcileLeaf[T](a : Option[T], b : Option[T]) : Option[T] =
  {
    if (a == b)                      a; // covers the None == None case
    else if (a != None && b == None) a;
    else if (a == None && b != None) b;
    else CantReconcileException.ouch( a, b ); 
  }

  def reconcileOverLeaf[T](a : Option[T], b : Option[T]) : Option[T] =
  {
    if (a == b)                      a; // covers the None == None case
    else if (a != None && b == None) a;
    else if (a == None && b != None) b;
    else a;
  }

  def reconcileLeaf[T](a : T, b : T) : T =
  {
    if (a == b)                      a; 
    else CantReconcileException.ouch( a, b ); 
  }

  def reconcileOverLeaf[T](a : T, b : T) : T = a;

  def reconcile[T<: Reconcilable[T]](a : Option[T], b : Option[T]) : Option[T] =
  {
    if (a == b)                      a; // covers the None == None case
    else if (a != None && b == None) a;
    else if (a == None && b != None) b;
    else 
      {
	val aVal : T = a.get;
	val bVal : T = b.get;
	Some(reconcile( aVal, bVal ));
      } 
  }

  def reconcileOver[T<: Reconcilable[T]](a : Option[T], b : Option[T]) : Option[T] =
  {
    if (a == b)                      a; // covers the None == None case
    else if (a != None && b == None) a;
    else if (a == None && b != None) b;
    else                           
      {
	val aVal : T = a.get;
	val bVal : T = b.get;
	Some(reconcileOver( aVal, bVal ));
      } 
  }


/*
  def reconcile[T](a : T, b : Option[T]) : Option[T] =
  { reconcile( Some(a), b); }

  def reconcile[T](a : Option[T], b : T) : Option[T] =
  { reconcile( a, Some(b) ); }
*/

  def reconcile[T<:Reconcilable[T]]( a : T, b : T) : T =
  {
    require(a.getClass == b.getClass)
    a.reconcile(b);
  }

  def reconcileOver[T<:Reconcilable[T]]( a : T, b : T) : T =
  {
    require(a.getClass == b.getClass)
    a.reconcileOver(b);
  }


/*
  def reconcile[T<:Reconcilable[T]]( a : T, b : T) : T =
  {
//    if (a.isInstanceOf[Reconcilable[T]] && b.isInstanceOf[Reconcilable[T]]
//	&& a.asInstanceOf[Reconcilable[T]].getClass == b.asInstanceOf[Reconcilable[T]].getClass)
    if (a.getClass == b.getClass)
//      a.asInstanceOf[Reconcilable[T]].reconcileOver(b.asInstanceOf[T])
      a.reconcile(b);
    else
      CantReconcileException.ouch( a, b );
  }

  def reconcileOver[T<:Reconcilable[T]]( a : T, b : T) : T =
  {
    if (a.getClass == b.getClass)
      a.reconcileOver(b);
    else
      CantReconcileException.ouch( a, b );
  }
*/   

  def reconcile[T <: Reconcilable[T]]( tVals : Iterable[T] ) : T =
  { reconcile( tVals.toSeq ); }

  def reconcile[T <: Reconcilable[T]]( cq : Seq[T] ) : T =
  {
    require(cq.size > 0);
    cq.reduceLeft( (a, b) => a reconcile b );
  }

  def reconcileOver[T <: Reconcilable[T]]( tVals : Iterable[T] ) : T =
  { reconcileOver( tVals.toSeq ); }

  def reconcileOver[T <: Reconcilable[T]]( cq : Seq[T] ) : T =
  {
    require(cq.size > 0);
    cq.reduceLeft( (a, b) => a reconcileOver b );
  }

}

