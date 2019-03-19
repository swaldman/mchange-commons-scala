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

  implicit class RichLong( val long : Long ) extends AnyVal {
    def toIntExact : Int = {
      if ( long.isValidInt ) {
        long.toInt
      } else {
        throw new ArithmeticException( s"${long}L cannot be converted to Int (not in range [${Integer.MIN_VALUE},${Integer.MAX_VALUE})" )
      }
    }
  }
}

