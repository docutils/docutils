<?php
#
# $Id$
# $Source$
#
# PHP script to call on the python CGI script.
#
# This is only an example, where a site has PHP enabled but not CGI.
#

$p = $_GET["p"];
$python = "/DLlocal/blais/httpd-soft/bin/python";
$env = 
"CONVERTER=\"$python ".
"/DLlocal/blais/httpd-soft/bin/html.py\" ".
"NO_CONTENT_TYPE=1 ".
"SCRIPT_NAME=$SCRIPT_NAME ";

system("$env $python rst-server.cgi \"p=$p\" 2>&1 ");

?>
