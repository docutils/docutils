.. Note:: Sorry, wegen kaputter Formatierung, aber ich habe keine
   Ahnung, wie das geht und es gibt leider keine Hilfe. Ich verwende
   einfach mal reStructuredText_.

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

Diese Seite beschäftigt sich mit der Technik, die die
`Oekonux-Community`_ nutzt. Ausgelöst wurde dies durch einen `Thread
zum Thema
<http://www.oekonux.de/projekt/liste/archive/msg03333.html>`_ auf der
`Projekt-Liste`_, die sich um die Organisation des Projekts kümmert.

.. _Oekonux-Community: http://www.oekonux.de/
.. _Projekt-Liste: http://www.oekonux.de/projekt/liste/

Überblick
=========

Die Oekonux-Community nutzt derzeit die folgenden technischen
Einrichtungen:

* `Mailing-Listen`_

* `Web-Sites`_

* OpenTheory_

* `News-Groups`_

* Wiki_

Weitere Einrichtungen wie IRC werden nach meiner Kenntnis derzeit
nicht genutzt.

Mailing-Listen
==============

Existierende Listen
~~~~~~~~~~~~~~~~~~~

Folgende Listen existieren:

* ``liste AT oekonux DOT de``

* ``list-en AT oekonux DOT org``

* ``projekt AT oekonux DOT de``

* ``chat AT oekonux DOT de``

* ``mitglieder AT oekonux DOT de``

* ``helfer AT oekonux-konferenz DOT de``

* ``referenten AT oekonux-konferenz DOT de``

Für alle Mailing-Listen existieren Archive_, die auf der Web-Site
stehen, zu deren Domain sie gehören.

Hosting
~~~~~~~

Außer ``liste AT oekonux DOT de`` werden alle Mailing-Listen bei
StefanMn gehostet. Für diese sind POP-Boxen eingerichtet, die
regelmäßig gepollt werden. Diese Mailing-Listen werden von einem
Majordomo_ betreut.

.. _Majordomo: http://www.greatcircle.com/majordomo/

.. important:: Da Majordomo_ keine völlig Freie Software ist, gab es
   den Vorschlag eine Alternative zu benutzen. Vorschläge waren Sympa
   oder SmartList. Mit einer ganz neuen Lösung würde vermutlich
   MailMan_ zum Einsatz kommen.

``liste AT oekonux DOT de`` wird aus historischen Gründen von buug_
gehostet. Dort kommt seit einiger Zeit MailMan_ als
Verwaltungs-Software zum Einsatz.

.. _buug: http://post.openoffice.de/

.. _MailMan: http://www.list.org/

.. important:: In einer integrierten Lösung mit einem dedizierten
   Server könnten und sollten auch die Mailing-Listen auf diesem
   Server gehostet werden.

Features
~~~~~~~~

Spam-Schutz
-----------

Mail an sämtliche Mailing-Listen wird über einen SpamAssassin_
geleitet. Dies ist absolut unverzichtbar, da die Adressen der
Mailing-Listen mittlerweile sehr weit verbreitet sind. Die
SpamAssassin-Installation benötigt gelegentliche Wartung um gegen die
neuesten Ideen der SpammerInnen gewappnet zu sein.

.. _SpamAssassin: http://www.spamassassin.org/

Web-Sites
=========

Hosting
~~~~~~~

Provider
--------

Im Moment bezahlt der `e.V.`_ bei `1&1`_ ein `Premium-Paket
<http://hosting.1und1.de/xml/static?__page=premium>`_.

.. _e.V.: http://www.oekonux.de/projekt/verein/

.. _1&1: http://hosting.1und1.de/

Die Möglichkeiten dieses Pakets treffen nicht ganz die Bedürfnisse.
Einerseits beinhaltet es ungenutzte Features (z.B. Datenbank,
in2site_), andererseits fehlen erwünschte Features (Möglichkeit für
lokale Suchmaschine, die auf dem Server den Index erstellt).

.. important:: Es gibt ein `Angebot
   <http://www.oekonux.de/projekt/liste/archive/msg03128.html>`_ von
   Hostsharing_ e.G., die das Hosting übernehmen könnten.

.. _Hostsharing: http://www.hostsharing.net/

Die Audio-Dateien, die vor allem bei den Konferenzen aufgezeichnet
worden sind, liegen wegen ihrer Größe auf anderen Servern.

Mit Ressourcen aus dem `OpenTheory-Projekt`_ wird im Vorfeld der
Konferenz das Programm der Konferenz online erstellt und präsentiert.

.. _OpenTheory-Projekt: http://www.opentheory.org/

.. important:: Es gibt Überlegungen, alle von der Oekonux-Community
   genutzten Ressourcen zu zentralisieren. Dazu würde vermutlich ein
   dedizierter Server benötigt.

.. important:: Als weiterer Hoster käme das `Individual Network
   Berlin`_ in Frage.

   `vLinux.de`_ oder server4you_ könnten weitere, vor allem preisgünstige
   Alternative sein.

.. _Individual Network Berlin: http://www.in-berlin.de/

.. _vLinux.de: http://vlinux.de/

.. _server4you: http://www.server4you.de/de/v/

.. important:: Holger hat sich grundsätzlich bereit erklärt, einen
   dedizierten Server zu betreuen.

Domains
-------

Die fünf Domains, die auf den Verein laufen, sind:

* ``oekonux.de``

  Unter ``www`` liegt hier die deutsche Hauptseite des Projekts.

* ``oekonux.org``

  Unter ``www`` liegt hier die internationale Hauptseite des Projekts.

* ``oekonux-konferenz.de``

  Die deutsche Domain für alles rund um die Oekonux-Konferenzen.

  - ``www``

    Einstiegsseite für die jeweils aktuelle Konferenz.

  - ``erste``, ``zweite``, ``dritte``

    Einstiegsseiten für die jeweilige spezifische Konferenz.

* ``oekonux-konferenz.org``, ``oekonux-conference.org``

  Die internationalen Domains für alles rund um die
  Oekonux-Konferenzen.

  - ``www``

    Einstiegsseite für die jeweils aktuelle Konferenz.

  - ``second``, ``third``

    Einstiegsseiten für die jeweilige spezifische Konferenz.

  .. important:: ``oekonux-konferenz.org`` kann wahrscheinlich
     mittelfristig entfallen.

Weitere Domains waren vor der Übernahme durch den `e.V.`_ vorhanden,
diese sind aber zurück gegeben worden.

Features
~~~~~~~~

Web-Space
---------

Derzeit sind in allen vier Web-Präsenzen insgesamt knapp 200MB von ca.
17000 Files belegt. Die extern gehosteten Audio-Files umfassen ca.
1GB.

Traffic
-------

Derzeit generieren die Web-Sites ca. 2GB Traffic pro Monat.

Formulare
---------

Die von `1&1`_ angebotenen so genannten Web-Elements werden im Vorfeld
einer Konferenz für Anmeldungsformulare verwendet.

Weitere Formulare werden nicht genutzt.

Mail-Adressen
-------------

Derzeit verteilen sich rund 80 Mail-Adressen auf die verschiedenen
Domains_. Nur ein Teil von ihnen ist als POP-Box realisiert -
insbesondere die Adressen, die für die `Mailing-Listen`_ verwendet
werden. Weitere Mail-Adressen sind als Alias eingerichtet.

Neben den funktionsbezogenen Mail-Adressen gibt es derzeit lediglich
eine persönliche Mail-Adresse in den Domains_. Grundsätzlich können
weitere private Mail-Adressen vergeben werden. Vor langer Zeit wurde
beschlossen, dass der Nachname der Person in der Mail-Adresse
enthalten sein muss.

Generierung und Upload
----------------------

Derzeit werden alle Inhalte von StefanMn u.a. aus SDF-Quellen auf
seinem privaten Rechner generiert und per FTP auf die Web-Sites
geladen. Das Ganze ist in eine ``make``\ -basierte Automatisierung
eingebettet.

.. important:: Die Quellen für die statischen Anteile der jetzigen
   Lösung sind vor Ewigkeiten mal in ein CVS-Repository gestellt
   worden. Allerdings hat sich niemensch jemals dafür interessiert -
   geschweige denn Hilfe angeboten.

   Es wäre aber grundsätzlich gut, wenn eine neue Lösung für die
   Web-Sites mittels CVS (oder Subversion) zugänglich wäre. Damit
   könnten viele Leute an der Site arbeiten.

   StefanMn hätte gerne, dass die Quellen der Web-Seiten mittels einem
   ASCII-nahen Format - möglichst reStructuredText_ - erstellt werden
   könnten. Er würde (auch) dafür einen Konverter vom jetzigen Format
   zur Verfügung stellen.

Statische Seiten
----------------

Praktisch sämtliche Seiten in den Oekonux-Domains sind statische
HTML-Seiten ohne jegliches JavaScript oder andere aktive Elemente. Sie
sind damit

* für *alle* Browser zugänglich

* wird für alle Zeiten lesbar sein

* kein Sicherheitsproblem für den Server

* stabile URLs ohne Variablen

  Viele Suchmaschinen ignorieren Links mit ``?``.

* bedürfen keinerlei Wartung

* freundlich für externe Suchmaschinen

  Zwar werden für die Navigation Frames eingesetzt, es gibt jedoch
  immer einen ``<noframes>``\ -Bereich, der die entsprechenden Links
  enthält.

* mit Tools wie z.B. ``wget`` komplett und problemlos zu saugen

* funktioniert mit jedem Web-Hosting-Angebot

Privacy
-------

`1&1`_ stellt praktisch in allen Tarifen die Log-Files des Web-Servers
zur Verfügung. Darin sind die Zugriffe auf die einzelnen Domains
unterschieden. Diese Logs werden ausgewertet und ohne die zugreifenden
IP-Adressen und weitere, tendenziell Privacy-gefährdende Details auf
den Web-Seiten zur Verfügung gestellt.

.. important:: Wie StefanMn in einer `Mail an [pox]
   <http://www.oekonux.de/projekt/liste/archive/msg02683.html>`_
   vorgeschlagen hat, sollte die Privacy-Policy auf den Web-Sites
   veröffentlicht sein.

   Insbesondere sollte klar gemacht werden, dass in2site_ **nicht**
   verwendet wird.

.. _in2site: http://www.in2site.de/

Navigation
----------

Die Navigation wird mit Hilfe von Frames realisiert. Ein schmaler
Frame auf der linken Seite ist einem File-Browser nachempfunden. Er
zeigt ständig die aktuelle Position innerhalb der Site an. Der rechte
Frame enthält den angewählten Inhalt. Wird in der Navigation ein
Verzeichnis angewählt, so erscheint im Inhalts-Frame stets
``default.html``.

Außerdem hat jede Third-Level-Domain eine eigene Sitemap.
Links auf allen Seiten werden von QBullets_ gefolgt, die grob den Typ
des Links angeben.

.. _QBullets: http://www.matterform.com/qbullets/

Lokale Suchmaschine
-------------------

Mittels FreeFind_ wurde auf ``www.oekonux.de`` eine lokale,
kostenlose, durch Werbung finanzierte Suchmaschine eingerichtet.

.. _FreeFind: http://www.freefind.com/

.. important:: Nachdem das Größenlimit für kostenlosen Service im
   2003-05 bereits zum zweiten Mal überschritten wurde, indexiert
   diese Suchmaschine seit einiger Zeit nicht mehr die gesamte Site.

   Eine lokale Suchmaschine wie `Perlfect Search`_ scheitert daran,
   dass im derzeitigen Tarif einem Prozess zu wenig Rechenzeit zur
   Verfügung steht um die Site zu indexieren.

   Es besteht Handlungsbedarf.

.. _Perlfect Search: http://perlfect.com/freescripts/search/

RSS-Feed
--------

.. important:: Es wäre nett, einen RSS-Feed auf einem eigenen Server
   zu haben.

Datenbanken
-----------

.. important:: Derzeit werden Datenbanken nicht genutzt, aber es wäre
   sinnvoll, diese Option bei einer neuen Lösung zu haben.
   (vorzugsweise mySGL).

Apache
------

.. important:: Eine neue Hosting-Lösung sollte uns in möglichst großem
   Umfang Zugang zum Web-Server geben.

Inhalte
~~~~~~~

Allgemeines
-----------

.. important:: Generell werden die Sites nicht sehr intensiv gepflegt.
   Dies ist nicht gut.

.. important:: Insbesondere wäre es super, wenn die internationale
   Site mehr von den Inhalten der deutschen Site hätte. Besonders
   wünschenswert wäre es, wenn die `Oekonux-Links`_ auf Englisch
   verfügbar wären.

Archive
-------

Sämtliche `Mailing-Listen`_ des Projekts werden auf den Web-Sites
archiviert. Die Mails werden bei Eingang bei StefanMn mittels MHonArc_
in HTML konvertiert und automatisch hochgeladen.

.. _MHonArc: http://www.mhonarc.org/

Die HTML-Seiten für die Mails werden noch gefiltert. Momentan werden

* Kommentare von MHonArc_ entfernt

* Telefon-Nummern

* Einzelne Personen, die sich zu spät entschieden haben, dass sie
  nicht per Google bei Oekonux gefunden werden wollen

* Alle URLs, die auf Mail-Adressen verweisen

* Mail-Adressen werden unkenntlich gemacht

.. important:: Weiter werden ASCII-Äquivalente der Archive in Paketen
   zu 40 Stück auf die Web-Site gestellt. Dieses Feature ist obsolet.

.. important:: Es besteht der Wunsch, die Archive auch im mbox-Format
   auf die `Web-Sites`_ zu bringen. Hier muss noch überlegt werden,
   was gegen Mail-Adressen-Harvesting getan werden kann.

Das Löschen aus den Archiven ist ebenfalls weit gehend automatisiert.
Es erfordert aber einen expliziten Aufruf, der die Nummer der
entsprechenden Mail nennt.

.. important:: Momentan wird die gelöschte Mail nur aus den Indexen
   entfernt, sie wird aber *nicht* vom Server gelöscht. Dadurch ist
   sie zwar nicht mehr über Indexe oder benachbarte Mails verlinkt,
   bleibt aber grundsätzlich zugreifbar. Das muss geändert werden.

Texte
-----

Verschiedene Texte des Projekts sind auf den Web-Sites verfügbar.

Darunter befindet sich eine Einführung, deren Quellen auch für
MagicPoint vorliegen

Link-Seite
----------

Eine wichtige inhaltliche Ressource der Oekonux-Community sind die
`Oekonux-Links`_, in die interessante Links mit einem kurzen Kommentar
aufgenommen werden. Die Seite ist strukturiert.

.. _Oekonux-Links: http://www.oekonux.de/projekt/links.html

.. important:: Es wäre sehr gut, wenn jeder der `Oekonux-Links`_ das
   Datum bekäme, zu dem er erstellt wurde. Mittlerweile hätte dies
   einen gewissen dokumentarischen Wert ("Aha, damals war das also von
   Bedeutung").

   Dies wäre mittels ``cvs annotate`` grundsätzlich auch für schon
   vorhandene Links machbar.

.. important:: Es wäre gut, wenn die Sprache der Site angegeben wäre,
   die durch den Link referenziert wird. Dies ist mittels des
   HTML-Attributs ``hreflang`` machbar.

Statistiken
-----------

Statistiken sowohl über die Nutzung der `Mailing-Listen`_ als auch
über die Nutzung der `Web-Sites`_ werden auf den `Web-Sites`_ zur
Verfügung gestellt. Alle Statistiken werden mit Freien Tools
(webalizer_, mail2clf_, mail2chart_) mindestens einmal wöchentlich von
StefanMn automatisch erstellt und hochgeladen. Die
Web-Site-Statistiken werden einmal monatlich um Referrer-Links
bereinigt, die von Porno-Sites stammen.

.. _webalizer: http://www.webalizer.org/

.. _mail2clf: http://www.merten-home.de/FreeSoftware/mail2clf/

.. _mail2chart: http://www.merten-home.de/FreeSoftware/mail2chart/

OpenTheory
==========

Viele Texte aus und rund um das Oekonux-Projekt sind auch im
`OpenTheory-Projekt`_ verfügbar. Einige sind im
`Oekonux-OpenTheory-Projekt`_ zusammen gefasst.

.. _Oekonux-OpenTheory-Projekt: http://www.opentheory.org/oekonux/

Die Texte sind sowohl inhaltlicher als auch organisatorischer Natur.
Insbesondere für die Vorbereitung der Konferenzen wurden
OpenTheory-Projekte angelegt.

Wiki
====

Derzeit nutzt die Oekonux-Community zwei Wiki-Installationen:

* ``co-forum.de``

  Wird technisch von Thomas Kalka betreut und auch gehosteten. Es
  handelt sich um ein allgemeines Wiki.

* ``de.wiki.oekonux.org.uk``

* ``en.wiki.oekonux.org.uk``

  Diese beiden Wikis sind neueren Datums und explizit als Oekonux-Wiki
  gedacht. Sie werden technisch von Chris Croome betreut und auch
  gehostet.

  .. important:: Bislang besteht keine inhaltliche Betreuung.

News-Groups
===========

Seit Ende 2003 werden die Haupt-\ `Mailing-Listen`_ für die Diskussion
auch mittels Gmane_ als Usenet-Newsgroups angeboten:

.. _Gmane: http://gmane.org/

* ``news://news.gmane.org/gmane.politics.oekonux.german``

  Spiegelt ``liste AT oekonux DOT de``.

* ``news://news.gmane.org/gmane.politics.oekonux.english``

  Spiegelt ``list-en AT oekonux DOT org``.

Die News-Groups sind so eingestellt, das Antworten über die News nicht
möglich sind.

.. important:: Zum heutigen Zeitpunkt (2004-01-23) enthalten die
   News-Group-Archive nur die Mails, die geschrieben wurde, nachdem
   das News-Group-Feature hinzu genommen wurde. Nachdem das Problem
   der lesbaren Mail-Adressen gelöst wurde, ist es angestrebt, dass
   die gesamten Archive in das News-Group-Archiv `importiert
   <http://gmane.org/import.php>`_ werden.

Features
~~~~~~~~

Suchmaschine
------------

Gmane_ verfügt über eine Suchmaschine, die die entsprechenden
News-Archive durchsuchen.

.. important:: Zumindest übergangsweise wäre es gut, diese
   Suchmaschine direkt über die `Web-Sites`_ zu verlinken oder
   anderweitig zu integrieren.

..  LocalWords:  reStructuredText Premium page premium oekonux www org Hoster
..  LocalWords:  konferenz conference second third gehosteten MagicPoint Upload
..  LocalWords:  SDF make MHonArc Harvesting ToDo important JavaScript noframes
..  LocalWords:  wget pox Privacy Policy site default html Sitemap FreeFind you
..  LocalWords:  Perlfect clf Search webalizer mail mail chart Referrer co chat
..  LocalWords:  forum Kalka Chris Croome Gmane DOT list en mbox Subversion cvs
..  LocalWords:  StefanMn projekt helfer referenten gepollt Sympa SmartList
..  LocalWords:  MailMan buug SpamAssassin Individual Network vLinux server
..  LocalWords:  Repository QBullets annotate hreflang
