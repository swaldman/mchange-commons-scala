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

package com.mchange.sc.v1.parsing;

import scala.util.parsing.combinator.RegexParsers;
  
class CsvParser extends RegexParsers
{
  override def skipWhitespace = false;

  def EOL : Parser[String]               = "\r" ||| "\n" ||| "\r\n"; 
  def datum : Parser[String]             = "" ||| ("""[^\042\r\n,][^\r\n,]*""".r | """(?s)\042.*?(?<!\\)\042""".r ^^ ( qs => qs.substring(1, qs.length-1) )) ;
  def lineData : Parser[List[String]]    = rep1sep( datum, ",");
  def line : Parser[List[String]]        = lineData <~ opt(EOL);
  def lines : Parser[List[List[String]]] = repsep(lineData, EOL) <~ opt(EOL);
}
