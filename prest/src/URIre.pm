# $Id: URIre.pm 768 2006-01-28 03:33:28Z marknodine $
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the GNU General Public License (GPL).

package URIre;

BEGIN {
    $digit         = "\\d";
    $alpha         = "[a-zA-Z]";
    $alphanum      = "[a-zA-Z0-9]";
    $hex           = "[0-9a-fA-F]";
    $escaped       = "(?:\%${hex}{2}|\\\\.)";
    $mark          = "[-_.!~*\'()]";
    $unreserved    = "(?:$alphanum|$mark)";
    $reserved      = "[;/?:\@&=+\$,]";
    $uric          = "(?:$reserved|$unreserved|$escaped)";
    $fragment      = "(?:$uric)*";
    $query         = "(?:$uric)*";
    $pchar         = "(?:$unreserved|$escaped|[:\@&=+\$,])";
    $param         = "(?:$pchar)*";
    $segment       = "(?:$pchar)*(?:;$param)*";
    $path_segments = "$segment(?:/$segment)*";
    $port          = "(?:$digit)*";
    $IPv4address   = "$digit\\.$digit\\.$digit\\.$digit";
    $IPv6address   = "\\[(?:$hex*:)+$hex*\\]";
    $toplabel      = "(?:$alpha|$alpha(?:$alphanum|-)*$alphanum)";
    $domainlabel   = "(?:$alphanum|$alphanum(?:$alphanum|-)*$alphanum)";
    $hostname      = "(?:$domainlabel\\.)*$toplabel\\.?";
    $host          = "(?:$hostname|$IPv4address|$IPv6address)";
    $hostport      = "$host(?::$port)?";
    $userinfo      = "(?:$unreserved|$escaped|[;:&=+\$,])*";
    $server        = "(?:(?:$userinfo\@)?$hostport)?";
    $reg_name      = "(?:$unreserved|$escaped|[\$,;:\@&=+])";
    $authority     = "(?:$server|$reg_name)";
    $scheme        = "$alpha(?:$alpha|$digit|[-+.])*";
    $rel_segment   = "(?:$unreserved|$escaped|[;\@&=+\$,])";
    $abs_path      = "/$path_segments";
    $rel_path      = "$rel_segment(?:$abs_path)?";
    $net_path      = "//$authority(?:$abs_path)?";
    $uric_no_slash = "(?:$unreserved|$escaped|[;?:\@&=+\$,])";
    $opaque_part   = "$uric_no_slash(?:$uric)*";
    $path          = "(?:$abs_path|$opaque_part)?";
    $hier_part     = "(?:$net_path|$abs_path)(?:\\?$query)?";
    $absoluteURI   = "(?:$scheme:(?:$hier_part|$opaque_part))";
    $relativeURI   = "(?:$net_path|$abs_path|$rel_path)(?:\\?$query)?";
    $URI_reference = "(?:$absoluteURI|$relativeURI)(?:$fragment)?";
}

1;
