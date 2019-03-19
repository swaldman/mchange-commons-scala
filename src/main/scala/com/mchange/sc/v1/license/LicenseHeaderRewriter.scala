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

package com.mchange.sc.v1.license;

import java.io._;
import scala.io.Source;
import scala.io.Codec;

import com.mchange.v2.io.FileUtils.findRelativeToParent;
import com.mchange.sc.v1.util.ClosableUtils.withClosable;

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
