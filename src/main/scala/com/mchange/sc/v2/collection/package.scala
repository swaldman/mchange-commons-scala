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
