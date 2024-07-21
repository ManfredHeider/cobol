      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0536.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0536.CPv  $
      *
      *   Rev 2.2   Jul 24 2013 16:38:32   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jun 28 2013 15:03:58   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0536.
000020*    Eingabeparamter UPAT0062
000030        07 PROGRAMMNAME           PICTURE X(8).
000040*    Programmname
000050        07 SQL-SCHNITTSTELLE.
000060*    SQL Communications Area ( SQLCA )
000070           09 SQLAID              PICTURE X(8).
000080*    SQLAID
000090           09 SQLABC              PICTURE S9(9)  COMPUTATIONAL.
000100*    SQLABC
000110           09 AUS-SQLCODE         PICTURE S9(9)  COMPUTATIONAL.
000120*    SQLCODE
000130           09 SQLERRM.
000140*    SQLERRM ( Bestandteil der SQLCA )
000150              11 SQLERRML         PICTURE S9(4)  COMPUTATIONAL.
000160*    SQLERRML
000170              11 SQLERRMC         PICTURE X(70).
000180*    SQLERRMC
000190           09 SQLERRP             PICTURE X(8).
000200*    SQLERRP
000210           09 SQLERRD             OCCURS 6 TIMES
000220                                  PICTURE S9(9)  COMPUTATIONAL.
000230*    SQLERRD
000240           09 SQLWARN.
000250*    SQLWARN ( Bestandteil der SQLCA )
000260              11 SQLWARN0         PICTURE X.
000270*    SQLWARN0
000280              11 SQLWARN1         PICTURE X.
000290*    SQLWARN1
000300              11 SQLWARN2         PICTURE X.
000310*    SQLWARN2
000320              11 SQLWARN3         PICTURE X.
000330*    SQLWARN3
000340              11 SQLWARN4         PICTURE X.
000350*    SQLWARN4
000360              11 SQLWARN5         PICTURE X.
000370*    SQLWARN5
000380              11 SQLWARN6         PICTURE X.
000390*    SQLWARN6
000400              11 SQLWARN7         PICTURE X.
000410*    SQLWARN7
000420           09 SQLEXT.
000430*    SQLEXT ( Bestandteil der SQLCA )
000440              11 SQLWARN8         PICTURE X.
000450*    SQLWARN8
000460              11 SQLWARN9         PICTURE X.
000470*    SQLWARN9
000480              11 SQLWARNA         PICTURE X.
000490*    SQLWARNA
000500              11 AUS-SQLSTATE     PICTURE X(5).
000510*    SQLSTATE
