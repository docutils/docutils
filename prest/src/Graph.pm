package Graph;

# $Id$
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the GNU General Public License (GPL).

# This package contains routines for representing and manipulating
# graph objects.

# Data structures:
#   _`Graph` object: A hash reference.  The hash is accessed through
#                    two levels of indices representing the
#                    x-coordinate and the y-coordinate of a vertex in
#                    the graph.  The next level of indexing is a hash
#                    reference with special keys
#
#                      ``edges``: an array of point objects that are
#                                 incident to the vertex
#                      ``props``: a hash of user-defined properties 
#                                 on the vertex
#   _`Point` object: A reference to an array with two elements giving
#                    the x and y coordinates of the point.

use strict;

# CLASS METHOD.
# Creates a new graph object.
# Returns: reference to a graph object
sub new {
    return bless {};
}

# INSTANCE METHOD.
# Adds an edge to the graph.
# Arguments: point object 1, point object 2
# Returns: None
sub AddEdge {
    my($g, $p1, $p2) = @_;

    push(@{$g->{$p1->[0]}{$p1->[1]}{edges}}, $p2);
    push(@{$g->{$p2->[0]}{$p2->[1]}{edges}}, $p1);
}

# INSTANCE METHOD.
# Sets a user-defined property for a vertex.
# Arguments: point object, property name, property value
# Returns: None
sub SetVertexProp {
    my($g, $p, $prop, $val) = @_;

    $g->{$p->[0]}{$p->[1]}{props}{$prop} = $val;
}

# INSTANCE METHOD.
# Gets a user-defined property for a vertex.
# Arguments: point object, property name
# Returns: Property value
sub GetVertexProp {
    my($g, $p, $prop) = @_;

    return $g->{$p->[0]}{$p->[1]}{props}{$prop};
}

# INSTANCE METHOD.
# Gets the list of vertices in the graph.
# Arguments: None
# Returns: array of point objects
sub GetVertices {
    my($g) = @_;

    my @verts;
    my @vs = sort {$a <=> $b} keys %$g;
    my $v;
    foreach $v (@vs) {
	my @hs = sort {$a <=> $b} keys %{$g->{$v}};
	push(@verts, map([$v,$_], @hs));
    }
    return @verts;
}

# INSTANCE METHOD.
# Gets a list of vertices adjacent to a vertex.
# Arguments: point object
# Returns: array of point objects
sub GetVertexEdges {
    my($g, $p) = @_;

    return @{$g->{$p->[0]}{$p->[1]}{edges}};
}

# INSTANCE METHOD.
# Does a Depth-First Search of a graph from a given starting point.
# Arguments: point object, callback routine
# Returns: None
# Callback routine arguments: graph, point
# Callback routine returns: None
sub DFS {
    my($g, $p, $sub) = @_;
    # First reset the 'dfs' entry on all vertices
    my @verts = $g->GetVertices();
    foreach (@verts) {
	$g->{$_->[0]}{$_->[1]}{dfs} = 0;
    }
    _DFS(@_);
}

# Internal routine. Called recursively by DFS.
sub _DFS {
    my($g, $p, $sub) = @_;
    &$sub($g, $p);
    $g->{$p->[0]}{$p->[1]}{dfs} = 1;
    my @edges = $g->GetVertexEdges($p);
    my $p2;
    foreach $p2 (@edges) {
	$g->_DFS($p2, $sub) unless $g->{$p2->[0]}{$p2->[1]}{dfs};
    }
}

1;
