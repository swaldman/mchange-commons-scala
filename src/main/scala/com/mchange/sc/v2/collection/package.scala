package com.mchange.sc.v2;

import scala.collection._
import scala.collection.{immutable => scimmutable}

import com.mchange.leftright.BiasedEither;

import BiasedEither.RightBias._

package object collection {

  /**
    * Returns as Right the successfully constructed set if there were no duplicates,
    * as Left the first duplicate value encountered if there were duplicates
    */ 
  def toSetIfNoDups[T]( iterable : Iterable[T] ) : Either[T,scimmutable.Set[T]] = {
    iterable.foldLeft(Right(scimmutable.Set.empty[T]) : Either[T,scimmutable.Set[T]] ) { (accum, next) =>
      accum.flatMap( set => if (set.contains(next)) Left(next) else Right(set + next) )
    }
  }

  /**
    * Returns as Right the successfully constructed Map if there were no duplicate keys,
    * as Left the first duplicate key encounted if there were duplicate keys
    */ 
  def toMapIfNoDupKeys[K,V]( iterable : Iterable[(K,V)] ) : Either[K,scimmutable.Map[K,V]] = {
    iterable.foldLeft(Right(scimmutable.Map.empty[K,V]) : Either[K,scimmutable.Map[K,V]]) { (accum, next) =>
      accum.flatMap( m => if (m.contains(next._1)) Left(next._1) else Right(m + next) )
    }
  }

  /**
    * Returns as Right the successfully constructed SortedMap if there were no duplicate keys,
    * as Left the first duplicate key encounted if there were duplicate keys
    */ 
  def toSortedMapIfNoDupKeys[K,V]( iterable : Iterable[(K,V)] )( implicit ordering : Ordering[K] ) : Either[K,scimmutable.SortedMap[K,V]] = {
    iterable.foldLeft(Right(scimmutable.SortedMap.empty[K,V]) : Either[K,scimmutable.SortedMap[K,V]]) { (accum, next) =>
      accum.flatMap( m => if (m.contains(next._1)) Left(next._1) else Right(m + next) )
    }
  }
}
