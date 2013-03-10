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

package com.mchange.sc.v1.util;

import java.io._;
import scala.io.Source;
import scala.io.Codec;

import com.mchange.v2.io.FileUtils.findRelativeToParent;
import ClosableUtils.withClosable;

object LicenseHeaderRewriter {

  private val CRLF = util.Properties.lineSeparator;

  def rewriteSrcDir( srcDir : File, targetDir : File, filter : (File) => Boolean, licenseHeader : String, headerConverter : (String) => String, headerLineWhile : (String) => Boolean )( implicit codec : Codec ) : Unit = {
    require( srcDir.isDirectory, "Source dir %s is not a directory.".format( srcDir ) );
    require( targetDir.isDirectory, "Target dir %s is not a directory.".format( srcDir ) );

    // thanks to http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
    def findAllChildren( dir : File ) : Seq[File] = {
      val kids = dir.listFiles();
      kids ++ kids.filter( _.isDirectory ).flatMap( findAllChildren( _ ) ) 
    }

    val srcFiles = findAllChildren( srcDir ).filter( srcFile => ( (!srcFile.isDirectory) && filter( srcFile ) ) );
    srcFiles.foreach { 
      file => {
	val relFile = findRelativeToParent(srcDir, file);
	val destFile = new File( targetDir, relFile.getPath );
	val text = rewritten( file, licenseHeader, headerConverter, headerLineWhile )( codec );
	withClosable( () => new BufferedWriter( new OutputStreamWriter( new FileOutputStream( destFile ), codec.encoder ) ) ) {
	  writer => {
	    writer.write( text );
	    writer.flush();
	  }
	}
      }
    }
  }


  def rewritten( file : File, licenseHeader : String, headerConverter : (String) => String, headerLineWhile : (String) => Boolean )(implicit codec : Codec) : String = 
    rewritten( Source.fromFile( file )( codec ).getLines, licenseHeader, headerConverter, headerLineWhile )

  def rewritten( lines : Iterator[String], licenseHeader : String, headerConverter : (String) => String, headerLineWhile : (String) => Boolean ) : String = {
    val noEmpty = lines.dropWhile( _.trim.length == 0 ); // drop empty lines
    val noHeaders = noEmpty.dropWhile( headerLineWhile );
    val noExtraBlanks = noHeaders.dropWhile( _.trim.length == 0 ); // drop empty lines
    headerConverter( licenseHeader ) + CRLF + CRLF + noExtraBlanks.mkString( CRLF ) + CRLF
  }

  def identityHeaderConverter( licenseHeader : String ) = licenseHeader.trim;

  def starSurroundHeaderConverter( licenseHeader : String ) : String = {
    """[\r\n]+""".r.split( licenseHeader.trim ).map(" * " + _).mkString(CRLF+"""/*"""+CRLF, CRLF, CRLF + " */"+CRLF)
  }

  def starSurroundHeaderLineWhile( line : String ) : Boolean = {
    val trimmed = line.trim;
    (trimmed.startsWith("/*") && !trimmed.startsWith("/**")) || trimmed.startsWith("*") || trimmed.startsWith("*/");
  }
  
}
