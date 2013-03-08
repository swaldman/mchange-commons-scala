/*
 * Distributed as part of mchange-commons-scala
 *
 * Copyright (C) 2013 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1, as 
 * published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this software; see the file LICENSE.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

package com.mchange.sc.v1.lang;

object Union {
  // i think that, as a practical matter, this is probably not so useful

  // Miles Sabin's uboxed union type hack... see http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/
  type ¬[A] = A => Nothing;
  type ¬¬[A] = ¬[¬[A]];
  type v[T, U] = ¬[¬[T] with ¬[U]];
  type ~[T, U] = { type union[X] = ¬¬[X] <:< (T v U) };

  // def check[T : (String~Int)#union ]( arg : T ) = arg
}
