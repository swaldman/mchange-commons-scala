#!/bin/sh

exec scala "$0" "$@"
!#

import java.io.File;
import com.mchange.sc.v1.util.LicenceHeaderRewriter._;

val srcDir    = new File( args(0) );
val targetDir = new File( args(1) );

val filter = (file : File) => file.getPath.endsWith(".scala");

val licenseHeader = """

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


""";

val headerConverter = identityHeaderConverter;

val headerLineWhile = starSurroundHeaderLineWhile;

rewriteSrcDir( srcDir, targetDir : File, filter, licenseHeader, headerConverter, headerLineWhile );

println("Done.");
