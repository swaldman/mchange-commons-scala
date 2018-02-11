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
