"""Notes (i.e., me working things out...)

Given the following Python::

    def func(a,b=1,c='jim',d=None,e=[],f={'a':1},g=(1,2,3)):

the tree produced by compiler looks like::

    <Function> 'func' 'a' 'b' 'c' 'd' 'e' 'f' 'g'
      <Const> 1
      <Const> 'jim'
      <Name> 'None'
      <List>
      <Dict>
        <Const> 'a'
        <Const> 1
      <Tuple>
        <Const> 1
        <Const> 2
        <Const> 3 0

If one has::

    def func2(a,*args,**kargs):

one gets::

    <Function> 'func2' 'a' 'args' 'kargs' 3 None

and lastly::

    def func3((a,b,c),d):

gives::

    <Function> 'func3' 'a' 'b' 'c' 'd' 0 None


`compiler.misc` defines `flatten(tup)` - maybe I should try it?

"""


# compiler.transformer contains this useful set of comments:
#
#
# The output tree has the following nodes:
#
# Source Python line #'s appear at the end of each of all of these nodes
# If a line # doesn't apply, there will be a None instead.
#
# module:     doc, node
# stmt:       [ node1, ..., nodeN ]
# function:   name, argnames, defaults, flags, doc, codeNode
# lambda:     argnames, defaults, flags, codeNode
# classdef:   name, bases, doc, codeNode
# pass:
# break:
# continue:
# for:        assignNode, listNode, bodyNode, elseNode
# while:      testNode, bodyNode, elseNode
# if:         [ (testNode, suiteNode), ... ], elseNode
# exec:       expr1Node, expr2Node, expr3Node
# from:       modname, [ name1, ..., nameN ]
# import:     [ name1, ..., nameN ]
# raise:      expr1Node, expr2Node, expr3Node
# tryfinally: trySuiteNode, finSuiteNode
# tryexcept:  trySuiteNode, [ (exprNode, assgnNode, suiteNode), ... ], elseNode
# return:     valueNode
# const:      value
# print:      [ node1, ..., nodeN ] [, dest]
# printnl:    [ node1, ..., nodeN ] [, dest]
# discard:    exprNode
# augassign:  node, op, expr
# assign:     [ node1, ..., nodeN ], exprNode
# ass_tuple:  [ node1, ..., nodeN ]
# ass_list:   [ node1, ..., nodeN ]
# ass_name:   name, flags
# ass_attr:   exprNode, attrname, flags
# list:       [ node1, ..., nodeN ]
# dict:       [ (key1, val1), ..., (keyN, valN) ]
# not:        exprNode
# compare:    exprNode, [ (op, node), ..., (op, node) ]
# name:       name
# global:     [ name1, ..., nameN ]
# backquote:  node
# getattr:    exprNode, attrname
# call_func:  node, [ arg1, ..., argN ]
# keyword:    name, exprNode
# subscript:  exprNode, flags, [ sub1, ..., subN ]
# ellipsis:
# sliceobj:   [ node1, ..., nodeN ]
# slice:      exprNode, flags, lowerNode, upperNode
# assert:     expr1, expr2
#
# Compiled as "binary" ops:
# tuple:      [ node1, ..., nodeN ]
# or:         [ node1, ..., nodeN ]
# and:        [ node1, ..., nodeN ]
# bitor:      [ node1, ..., nodeN ]
# bitxor:     [ node1, ..., nodeN ]
# bitand:     [ node1, ..., nodeN ]
#
# Operations easily evaluateable on constants:
# <<:         exprNode, shiftNode
# >>:         exprNode, shiftNode
# +:          leftNode, rightNode
# -:          leftNode, rightNode
# *:          leftNode, rightNode
# /:          leftNode, rightNode
# %:          leftNode, rightNode
# power:      leftNode, rightNode
# unary+:     node
# unary-:     node
# invert:     node
