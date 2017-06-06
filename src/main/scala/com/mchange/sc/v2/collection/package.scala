package com.mchange.sc.v2;

import scala.collection._
import scala.collection.{immutable => scimmutable}

import com.mchange.sc.v2.yinyang._

import YinYang.YangBias._

package object collection {

  /**
    * Returns as Yang the successfully constructed set if there were no duplicates,
    * as Yin the first duplicate value encountered if there were duplicates
    */ 
  def toSetIfNoDups[T]( iterable : Iterable[T] ) : YinYang[T,scimmutable.Set[T]] = {
    iterable.foldLeft(Yang(scimmutable.Set.empty[T]) : YinYang[T,scimmutable.Set[T]] ) { (accum, next) =>
      accum.flatMap( set => if (set.contains(next)) Yin(next) else Yang(set + next) )
    }
  }

  /**
    * Returns as Yang the successfully constructed Map if there were no duplicate keys,
    * as Yin the first duplicate key encounted if there were duplicate keys
    */ 
  def toMapIfNoDupKeys[K,V]( iterable : Iterable[(K,V)] ) : YinYang[K,scimmutable.Map[K,V]] = {
    iterable.foldLeft(Yang(scimmutable.Map.empty[K,V]) : YinYang[K,scimmutable.Map[K,V]]) { (accum, next) =>
      accum.flatMap( m => if (m.contains(next._1)) Yin(next._1) else Yang(m + next) )
    }
  }

  /**
    * Returns as Yang the successfully constructed SortedMap if there were no duplicate keys,
    * as Yin the first duplicate key encounted if there were duplicate keys
    */ 
  def toSortedMapIfNoDupKeys[K,V]( iterable : Iterable[(K,V)] )( implicit ordering : Ordering[K] ) : YinYang[K,scimmutable.SortedMap[K,V]] = {
    iterable.foldLeft(Yang(scimmutable.SortedMap.empty[K,V]) : YinYang[K,scimmutable.SortedMap[K,V]]) { (accum, next) =>
      accum.flatMap( m => if (m.contains(next._1)) Yin(next._1) else Yang(m + next) )
    }
  }
}
