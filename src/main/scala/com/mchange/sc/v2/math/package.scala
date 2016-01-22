package com.mchange.sc.v2;

import scala.annotation.tailrec;

package object math {
  private final val ProductLogMin = -1/scala.math.E

  // straight outta Wikipedia... Halley's method
  // https://en.wikipedia.org/wiki/Lambert_W_function
  //
  // TODO: test more systematically than a couple of examples
  def productLog( z : Double, seed : Double = 0, precision : Double = 0.00001, maxIterations : Int = 10000 ) : Double = {
    require( z > ProductLogMin && !z.isInfinite, s"productLog is not defined for z <= -1/e (${ProductLogMin}) or infinite z, so not defined for z = ${z}" )

    def computeNext( wj : Double ) : Double = {
      val ewj  = scala.math.exp( wj )
      val wjewj = wj * ewj
      val rhs = (wjewj - z) / ((ewj * (wj+1)) - (((wj+2)*(wjewj-z))/((2*wj) + 2)))
      wj - rhs
    }
    @tailrec
    def iterate( wj : Double, count : Int = 0 ) : Double = {
      val next = computeNext( wj );
      if ( scala.math.abs( next - wj ) < precision ) next
      else if ( count > maxIterations ) throw new Exception( s"productLog( ${z} ) failed to converge to a precision of ${precision} within ${maxIterations} iterations." )
      else iterate( next, count + 1 )
    }
    iterate( seed )
  }
  def optimalPrice( p0 : Double, p1 : Double, cost : Double, referencePrice : Double ) : Double = {
    val plArg = scala.math.exp( -1 + p0 + ((cost / referencePrice) * p1) )
    (-referencePrice + (cost * p1) - (referencePrice * productLog( plArg ))) / p1
  }
  def logitCdf2( p0 : Double, p1 : Double, x : Double ) : Double = {
    val exponent = -( p0 + (p1 * x) )
    1 / (1 + scala.math.exp( exponent ))
  }

  def lambertW( z : Double, seed : Double = 0, precision : Double = 0.00001, maxIterations : Int = 10000 ) : Double = {
    productLog( z, seed, precision, maxIterations )
  }
}

