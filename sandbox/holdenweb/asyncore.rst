==========
 asyncore
==========

-----------------------------
 Asynchronous socket handler
-----------------------------

Synopsis: A base class for developing asynchronous socket
          handling services.
Type: module builtin
Module-Author: Sam Rushing <rushing@nightmare.com>
Author: Christopher Petrilli <petrilli@amber.org>
Author: Steve Holden <sholden@holdenweb.com>

.. Type: ... builtin, standard, various others: any specific usages required?
..
.. Heavily adapted from original documentation by Sam Rushing.
..
..  ............................................
..   This is the (first) RFC822-reader strawman
..  ............................................
..    Presumes a custom reader appropriate to docpy
..    RFC822 continuation IS allowed (see Synopsis)
..    Needtocheck: RFC822-readers and multiple entities?  (Author lines)
..    Dunno about implication of \section in the original
..    Dunno about comments (#?);  "Credit: Sam Rushing?"
..    Note in passing: names of new roles and directives made similar to
..      the existing docpy macros on purpose (for existing corpus & community)
..    		    
..    Markups needed, used, and existing in rst:
..      *emphasis*
..
..    Markups needed, used, and modified by this strawman:
..      ``code``
..    
..    Roles needed below by this strawman:
..      :cfunction:``
..      :module:``
..      :refmodule:``
..      :class:``
..      :function:``
..      :var:``
..	:label:``
..    
..    Directives needed below by this strawman:
..      .. funcdesc::
..		need to parse for optional argumnents shown as [...]
..      .. classdesc::
..      .. datadesc::
..
..    TBS - formals, e.g., funcdesc - several alternatives proposed
..              below (see funcdesc) in this draft 
..          the one shown first seems on track for consensus 04.3.20
..              (the directive will parse brackets, etc. - easier to use!)

This module provides the basic infrastructure for writing asynchronous 
socket service clients and servers.

There are only two ways to have a program on a single processor do 
"more than one thing at a time." Multi-threaded programming is the 
simplest and most popular way to do it, but there is another very 
different technique, that lets you have nearly all the advantages of 
multi-threading, without actually using multiple threads.  It's really 
only practical if your program is largely I/O bound.  If your program 
is processor bound, then pre-emptive scheduled threads are probably what 
you really need. Network servers are rarely processor bound, however.

If your operating system supports the :cfunction:`select()` system call 
in its I/O library (and nearly all do), then you can use it to juggle 
multiple communication channels at once; doing other work while your 
I/O is taking place in the "background."  Although this strategy can 
seem strange and complex, especially at first, it is in many ways 
easier to understand and control than multi-threaded programming.  
The :module:`asyncore` module solves many of the difficult problems for 
you, making the task of building sophisticated high-performance 
network servers and clients a snap. For "conversational" applications
and protocols the companion  :refmodule:`asynchat` module is invaluable.

The basic idea behind both modules is to create one or more network
*channels*, instances of class :class:`asyncore.dispatcher` and
:class:`asynchat.async_chat`. Creating the channels adds them to a global
map, used by the :function:`loop()` function if you do not provide it
with your own :var:`map`.

Once the initial channel(s) is(are) created, calling the :function:`loop()`
function activates channel service, which continues until the last
channel (including any that have been added to the map during asynchronous
service) is closed.

.. funcdesc:: loop([timeout [, use_poll [, map]]])

    Enter a polling loop that only terminates after all open channels
    have been closed.  All arguments are optional.  The :var:`timeout`
    argument sets the timeout parameter for the appropriate
    :function:`select()` or :function:`poll()` call, measured in seconds;
    the default is 30 seconds.  The :var:`use_poll` parameter, if true,
    indicates that :function:`poll()` should be used in preference to
    :function:`select()` (the default is ``False``).  The :var:`map` parameter
    is a dictionary whose items are the channels to watch.  As channels
    are closed they are deleted from their map.  If :var:`map` is
    omitted, a global map is used (this map is updated by the default
    class :method:`__init__()`
    -- make sure you extend, rather than override, :method:`__init__()`
    if you want to retain this behavior).

    Channels (instances of :class:`asyncore.dispatcher`, :class:`asynchat.async_chat`
    and subclasses thereof) can freely be mixed in the map.
    
.. classdesc:: dispatcher()

    The :class:`dispatcher` class is a thin wrapper around a low-level socket object.
    To make it more useful, it has a few methods for event-handling  which are called
    from the asynchronous loop.  
    Otherwise, it can be treated as a normal non-blocking socket object.

    Two class attributes can be modified, to improve performance,
    or possibly even to conserve memory.

    .. datadesc:: ac_in_buffer_size
        The asynchronous input buffer size (default ``4096``).

    .. datadesc:: ac_out_buffer_size
        The asynchronous output buffer size (default ``4096``).

    The firing of low-level events at certain times or in certain connection
    states tells the asynchronous loop that certain higher-level events have
    taken place. For example, if we have asked for a socket to connect to
    another host, we know that the connection has been made when the socket
    becomes writable for the first time (at this point you know that you may
    write to it with the expectation of success). The implied higher-level
    events are:
  
    ===================           ===============================================
    ``Event``                     Description
    -------------------           -----------------------------------------------
    ``handle_connect()``          Implied by the first write event
    ``handle_close()``            Implied by a read event with no data available
    ``handle_accept()``           Implied by a read event on a listening socket
    ===================           ===============================================

  
    During asynchronous processing, each mapped channel's :method:`readable()`
    and :method:`writable()` methods are used to determine whether the channel's
    socket should be added to the list of channels :cfunction:`select()`\ ed or
    :cfunction:`poll()`\ ed for read and write events.

Thus, the set of channel events is larger than the basic socket events.
The full set of methods that can be overridden in your subclass follows:

.. methoddesc:: handle_read()
    Called when the asynchronous loop detects that a :method:`read()`
    call on the channel's socket will succeed.

.. methoddesc:: handle_write()
    Called when the asynchronous loop detects that a writable socket
    can be written.  
    Often this method will implement the necessary buffering for 
    performance.  For example::


        def handle_write(self):
            sent = self.send(self.buffer)
            self.buffer = self.buffer[sent:]

.. methoddesc:: handle_expt()
  Called when there is out of band (OOB) data for a socket 
  connection.  This will almost never happen, as OOB is 
  tenuously supported and rarely used.

.. methoddesc:: handle_connect()
  Called when the active opener's socket actually makes a connection.
  Might send a ``welcome'' banner, or initiate a protocol
  negotiation with the remote endpoint, for example.

.. methoddesc:: handle_close()
  Called when the socket is closed.

.. methoddesc:: handle_error()
  Called when an exception is raised and not otherwise handled.  The default
  version prints a condensed traceback.

.. methoddesc:: handle_accept()
  Called on listening channels (passive openers) when a  
  connection can be established with a new remote endpoint that
  has issued a :method:`connect() call for the local endpoint.

.. methoddesc:: readable()
  Called each time around the asynchronous loop to determine whether a
  channel's socket should be added to the list on which read events can
  occur.  The default method simply returns ``True``, 
  indicating that by default, all channels will be interested in
  read events.

.. methoddesc:: writable()
  Called each time around the asynchronous loop to determine whether a
  channel's socket should be added to the list on which write events can
  occur.  The default method simply returns ``True``, 
  indicating that by default, all channels will be interested in
  write events.

In addition, each channel delegates or extends many of the socket methods.
Most of these are nearly identical to their socket partners.

.. methoddesc:: create_socket(family, type)
  This is identical to the creation of a normal socket, and 
  will use the same options for creation.  Refer to the
  :refmodule:`socket` documentation for information on creating
  sockets.

.. methoddesc:: connect(address)
  As with the normal socket object, :var:`address` is a 
  tuple with the first element the host to connect to, and the 
  second the port number.

.. methoddesc:: send(data)
  Send :var:`data` to the remote end-point of the socket.

.. methoddesc:: recv(buffer_size)
  Read at most :var:`buffer_size` bytes from the socket's remote end-point.
  An empty string implies that the channel has been closed from the other
  end.

.. methoddesc:: listen(backlog)
  Listen for connections made to the socket.  The :var:`backlog`
  argument specifies the maximum number of queued connections
  and should be at least 1; the maximum value is
  system-dependent (usually 5).

.. methoddesc:: bind(address)
  Bind the socket to :var:`address`.  The socket must not already
  be bound.  (The format of :var:`address` depends on the address
  family --- see above.)

.. methoddesc:: accept()
  Accept a connection.  The socket must be bound to an address
  and listening for connections.  The return value is a pair
  ``(conn , address)`` where :var:`conn` is a
  *new* socket object usable to send and receive data on
  the connection, and :var:`address` is the address bound to the
  socket on the other end of the connection.

.. methoddesc:: close()
  Close the socket.  All future operations on the socket object
  will fail.  The remote end-point will receive no more data (after
  queued data is flushed).  Sockets are automatically closed
  when they are garbage-collected.


	asyncore Example basic HTTP client :label:`asyncore-example`
	------------------------------------------------------------
	As a basic example, below is a very basic HTTP client that uses the 
	:class:`dispatcher` class to implement its socket handling::

	class http_client(asyncore.dispatcher):
	    def __init__(self, host,path):
		asyncore.dispatcher.__init__(self)
		self.path = path
		self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
		self.connect( (host, 80) )
		self.buffer = 'GET %s HTTP/1.0\r\n\r\n' % self.path

	    def handle_connect(self):
		pass

	    def handle_read(self):
		data = self.recv(8192)
		print data

	    def writable(self):
		return (len(self.buffer) > 0)

	    def handle_write(self):
		sent = self.send(self.buffer)
		self.buffer = self.buffer[sent:]
