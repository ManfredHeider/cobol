      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0558.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0558.CPv  $
      *
      *   Rev 2.2   Jul 24 2013 16:39:06   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jun 28 2013 15:08:00   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DB2-TIMESTAMP.
000020*    Ausgabeparameter UPAT0074
000030        07 TIMESTAMP26            PICTURE X(26).
000040*    Timestamp26
000050        07 STATUS-BEREICH.
000060*    allgemeiner Status-Bereich (NEU)
000070           09 PROGRAMMNAME        PICTURE X(8).
000080*    Programmname
000090           09 RC                  PICTURE 99.
000100*    RC Returncode aus Unterprogrammen
000110           09 MELDUNGS-BESCHREIBUNG.
000120*    allgemeine Meldungs-Beschreibung
000130              11 MELDUNGS-NR      PICTURE 9(8).
000140*    Meldungsnummer
000150              11 SUB-VAR-1        PICTURE X(20).
000160*    Substitutionsvariable
000170              11 SUB-VAR-2        PICTURE X(20).
000180*    Substitutionsvariable
000190           09 FEHL-BER-ALT        PICTURE X(45).
000200*    Fehler für Online-Anwendungen
000210           09 FILLER              OCCURS 47 TIMES
000220                                  PICTURE X.
000230*    Filler-1
000240        07 SQL-BEREICH.
000250*    ergänzender SQL-Bereich OZPs (NEU)
000260           09 VIEWNAME            PICTURE X(18).
000270*    VIEWNAME
000280           09 OPERATION           PICTURE XXX.
000290     88 KST-SELECT                VALUE 'SEL'.
000300     88 KST-INSERT                VALUE 'INS'.
000310     88 KST-UPDATE                VALUE 'UPD'.
000320     88 KST-DELETE                VALUE 'DEL'.
000330     88 KST-START                 VALUE 'ST '.
000340     88 KST-BACKWARD              VALUE 'BW '.
000350     88 KST-FORWARD               VALUE 'FW '.
000360     88 KST-OPEN                  VALUE 'OPE'.
000370     88 KST-CLOSE                 VALUE 'CLO'.
000380     88 KST-FETCH                 VALUE 'FET'.
000390*    Operation
000400           09 AUS-SQLCA.
000410*    SQL Communications Area ( SQLCA )
000420              11 SQLAID           PICTURE X(8).
000430*    SQLAID
000440              11 SQLABC           PICTURE S9(9)  BINARY.
000450*    SQLABC
000460              11 AUS-SQLCODE      PICTURE S9(9)  BINARY.
000470*    SQLCODE
000480              11 SQLERRM.
000490*    SQLERRM ( Bestandteil der SQLCA )
000500                 13 SQLERRML      PICTURE S9(4)  BINARY.
000510*    SQLERRML
000520                 13 SQLERRMC      PICTURE X(70).
000530*    SQLERRMC
000540              11 SQLERRP          PICTURE X(8).
000550*    SQLERRP
000560              11 SQLERRD          OCCURS 6 TIMES
000570                                  PICTURE S9(9)  BINARY.
000580*    SQLERRD
000590              11 SQLWARN.
000600*    SQLWARN ( Bestandteil der SQLCA )
000610                 13 SQLWARN0      PICTURE X.
000620*    SQLWARN0
000630                 13 SQLWARN1      PICTURE X.
000640*    SQLWARN1
000650                 13 SQLWARN2      PICTURE X.
000660*    SQLWARN2
000670                 13 SQLWARN3      PICTURE X.
000680*    SQLWARN3
000690                 13 SQLWARN4      PICTURE X.
000700*    SQLWARN4
000710                 13 SQLWARN5      PICTURE X.
000720*    SQLWARN5
000730                 13 SQLWARN6      PICTURE X.
000740*    SQLWARN6
000750                 13 SQLWARN7      PICTURE X.
000760*    SQLWARN7
000770              11 SQLEXT.
000780*    SQLEXT ( Bestandteil der SQLCA )
000790                 13 SQLWARN8      PICTURE X.
000800*    SQLWARN8
000810                 13 SQLWARN9      PICTURE X.
000820*    SQLWARN9
000830                 13 SQLWARNA      PICTURE X.
000840*    SQLWARNA
000850                 13 AUS-SQLSTATE  PICTURE X(5).
000860*    SQLSTATE
000870           09 FILLER              OCCURS 93 TIMES
000880                                  PICTURE X.
000890*    Filler-1
