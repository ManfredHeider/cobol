      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0550.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0550.CPv  $
      *
      *   Rev 2.2   Jul 24 2013 16:38:48   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jun 28 2013 15:06:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * Member DRAT0550
      *
000010     05 SQL-BEREICH.
000020*    ergänzender SQL-Bereich OZPs (NEU)
000030        07 VIEWNAME               PICTURE X(18).
000040*    VIEWNAME
000050        07 OPERATION              PICTURE XXX.
000060*    Operation
000070        07 AUS-SQLCA.
000080*    SQL Communications Area ( SQLCA )
000090           09 SQLAID              PICTURE X(8).
000100*    SQLAID
000110           09 SQLABC              PICTURE S9(9)  COMPUTATIONAL.
000120*    SQLABC
000130           09 AUS-SQLCODE         PICTURE S9(9)  COMPUTATIONAL.
000140*    SQLCODE
000150           09 SQLERRM.
000160*    SQLERRM ( Bestandteil der SQLCA )
000170              11 SQLERRML         PICTURE S9(4)  COMPUTATIONAL.
000180*    SQLERRML
000190              11 SQLERRMC         PICTURE X(70).
000200*    SQLERRMC
000210           09 SQLERRP             PICTURE X(8).
000220*    SQLERRP
000230           09 SQLERRD             OCCURS 6 TIMES
000240                                  PICTURE S9(9)  COMPUTATIONAL.
000250*    SQLERRD
000260           09 SQLWARN.
000270*    SQLWARN ( Bestandteil der SQLCA )
000280              11 SQLWARN0         PICTURE X.
000290*    SQLWARN0
000300              11 SQLWARN1         PICTURE X.
000310*    SQLWARN1
000320              11 SQLWARN2         PICTURE X.
000330*    SQLWARN2
000340              11 SQLWARN3         PICTURE X.
000350*    SQLWARN3
000360              11 SQLWARN4         PICTURE X.
000370*    SQLWARN4
000380              11 SQLWARN5         PICTURE X.
000390*    SQLWARN5
000400              11 SQLWARN6         PICTURE X.
000410*    SQLWARN6
000420              11 SQLWARN7         PICTURE X.
000430*    SQLWARN7
000440           09 SQLEXT.
000450*    SQLEXT ( Bestandteil der SQLCA )
000460              11 SQLWARN8         PICTURE X.
000470*    SQLWARN8
000480              11 SQLWARN9         PICTURE X.
000490*    SQLWARN9
000500              11 SQLWARNA         PICTURE X.
000510*    SQLWARNA
000520              11 AUS-SQLSTATE     PICTURE X(5).
000530*    SQLSTATE
000540        07 FILLER                 OCCURS 93 TIMES
000550                                  PICTURE X.
000560*    Filler-1
