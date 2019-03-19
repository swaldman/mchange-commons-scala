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

package com.mchange.sc.v2.net

import scala.collection._

import java.net.URL
import java.util.concurrent.ThreadLocalRandom

object LoadBalancer {
  object Single {
    def apply[T : URLSource]( t : T ) : Single = Single( URLSource.toURL( t ) )
  }
  case class Single( url : URL ) extends LoadBalancer {
    def nextURL : URL = url
  }
  object Random {
    def apply[T : URLSource]( ts : Iterable[T] ) : Random = {
      Random( Vector.empty ++ ts.map( t => URLSource.toURL(t) ) )
    }
  }
  case class Random( urls : immutable.IndexedSeq[URL] ) extends LoadBalancer {
    require( urls.length > 0 )
    def nextURL : URL = urls( ThreadLocalRandom.current().nextInt( urls.length ) )
  }
  object RoundRobin {
    def apply[T : URLSource]( ts : Iterable[T] ) : RoundRobin = {
      RoundRobin( Vector.empty ++ ts.map( t => URLSource.toURL(t) ) )
    }
  }
  case class RoundRobin( urls : immutable.IndexedSeq[URL] ) extends LoadBalancer {
    require( urls.length > 0 )

    // MT: protected by this' lock
    private var next = 0

    def nextURL : URL = this.synchronized {
      val out = urls( next )
      next += 1
      out
    }
  }
}
trait LoadBalancer {
  def nextURL : URL
}
