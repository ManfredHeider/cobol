      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1579.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1579.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 18:01:46   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 09:10:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1577 - Erstellen Steuerdateien
      *
      *     SOURCE-NAME      :  DRMA1579
      *     DMR-NAME         :  DG-OV1579
      *
      *     AUTOR            :  WILHELM, OLIVER
      *     GENERIERUNG      :  07.08.00/09:34
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1579.
000020        07 STATUS-BEREICH.
000030           09 PROGRAMMNAME        PICTURE X(8).
000040           09 RC                  PICTURE 99.
000050           09 MELDUNGS-BESCHREIBUNG.
000060              11 MELDUNGS-NR      PICTURE 9(8).
000070              11 SUB-VAR-1        PICTURE X(20).
000080              11 SUB-VAR-2        PICTURE X(20).
000090           09 FEHL-BER-ALT        PICTURE X(45).
000100           09 FILLER              OCCURS 47 TIMES
000110                                  PICTURE X.
000120        07 SQL-BEREICH.
000130           09 VIEWNAME            PICTURE X(18).
000140           09 OPERATION           PICTURE XXX.
000150     88 KST-SELECT                VALUE 'SEL'.
000160     88 KST-INSERT                VALUE 'INS'.
000170     88 KST-UPDATE                VALUE 'UPD'.
000180     88 KST-DELETE                VALUE 'DEL'.
000190     88 KST-START                 VALUE 'ST '.
000200     88 KST-BACKWARD              VALUE 'BW '.
000210     88 KST-FORWARD               VALUE 'FW '.
000220     88 KST-OPEN                  VALUE 'OPE'.
000230     88 KST-CLOSE                 VALUE 'CLO'.
000240     88 KST-FETCH                 VALUE 'FET'.
000250           09 AUS-SQLCA.
000260              11 SQLAID           PICTURE X(8).
000270              11 SQLABC           PICTURE S9(9)  BINARY.
000280              11 AUS-SQLCODE      PICTURE S9(9)  BINARY.
000290              11 SQLERRM.
000300                 13 SQLERRML      PICTURE S9(4)  BINARY.
000310                 13 SQLERRMC      PICTURE X(70).
000320              11 SQLERRP          PICTURE X(8).
000330              11 SQLERRD          OCCURS 6 TIMES
000340                                  PICTURE S9(9)  BINARY.
000350              11 SQLWARN.
000360                 13 SQLWARN0      PICTURE X.
000370                 13 SQLWARN1      PICTURE X.
000380                 13 SQLWARN2      PICTURE X.
000390                 13 SQLWARN3      PICTURE X.
000400                 13 SQLWARN4      PICTURE X.
000410                 13 SQLWARN5      PICTURE X.
000420                 13 SQLWARN6      PICTURE X.
000430                 13 SQLWARN7      PICTURE X.
000440              11 SQLEXT.
000450                 13 SQLWARN8      PICTURE X.
000460                 13 SQLWARN9      PICTURE X.
000470                 13 SQLWARNA      PICTURE X.
000480                 13 AUS-SQLSTATE  PICTURE X(5).
000490           09 FILLER              OCCURS 93 TIMES
000500                                  PICTURE X.
000510        07 BUCHDAT                PICTURE X(10).
      *
      **************************************************************
