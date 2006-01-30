Test the various interpreted text roles.

:emphasis:`emphasis`

:literal:`**literal**`

:strong:`strong` :sub:`sub` :sup:`sup`
:subscript:`subscript` :superscript:`superscript`

x:sup:`2`\ y

-----------------

.. role:: b
   :prefix:
     :html: <b>
     :latex: \b{
   :suffix:
     :html: </b>
     :latex: }
.. role:: i
   :prefix:
     :html: <i>
   :suffix:
     :html: </i>
.. role:: u
   :prefix:
     :html: <u>
   :suffix:
     :html: </u>
.. role:: s
   :prefix:
     :html: <s>
   :suffix:
     :html: </s>
.. role:: strike
   :prefix:
     :html: <strike>
   :suffix:
     :html: </strike>
.. role:: tt
   :prefix:
     :html: <tt>
   :suffix:
     :html: </tt>

:b:`b` :i:`i` :u:`u` :s:`s` :strike:`strike` :tt:`tt`

.. role:: big
   :prefix:
     :html: <big>
   :suffix:
     :html: </big>
.. role:: small
   :prefix:
     :html: <small>
   :suffix:
     :html: </small>

(normal) :big:`big` :small:`small`

.. role:: bold(b)
.. role:: italic(i)
.. role:: underline(u)

:bold:`bold` :italic:`italic` :underline:`underline`

.. role:: red
   :prefix:
     :html: <font color="red">
   :suffix:
     :html: </font>
.. role:: size-2
   :prefix:
     :html: <font size="2">
   :suffix:
     :html: </font>
.. role:: helvetica
   :prefix:
     :html: <font face="helvetica">
   :suffix:
     :html: </font>

:red:`color red` :size-2:`size 2` :helvetica:`face helvetica`

.. role:: class-myclass
   :class: myclass
.. role:: href-nowhere
   :prefix:
     :html: <a href="http://www.nowhere.com">
   :suffix:
     :html: </a>

:class-myclass:`class myclass` :href-nowhere:`href to nowhere`

.. role:: orange
   :prefix:
     :html: <font color="orange">
   :suffix:
     :html: </font>
.. role:: yellow
   :prefix:
     :html: <font color="yellow">
   :suffix:
     :html: </font>
.. role:: green
   :prefix:
     :html: <font color="green">
   :suffix:
     :html: </font>
.. role:: blue
   :prefix:
     :html: <font color="blue">
   :suffix:
     :html: </font>
.. role:: navy
   :prefix:
     :html: <font color="navy">
   :suffix:
     :html: </font>
.. role:: purple
   :prefix:
     :html: <font color="purple">
   :suffix:
     :html: </font>

:red:`r`\ :orange:`a`\ :yellow:`i`\ :green:`n`\ :blue:`b`\ :navy:`o`\ :purple:`w`
