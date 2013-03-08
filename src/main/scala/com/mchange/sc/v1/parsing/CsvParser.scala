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
