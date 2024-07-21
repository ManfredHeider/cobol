      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1365.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1365.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 17:50:20   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 08:53:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1363 - Ermitteln Daten KISS
      *
      *     SOURCE-NAME      :  DRMA1365
      *     DMR-NAME         :  DG-OV1365
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  19.04.01/15:17
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1365.
000020        07 STATUS-SQL-BEREICH.
000030           09 STATUS-BEREICH.
000040              11 PROGRAMMNAME     PICTURE X(8).
000050              11 RC               PICTURE 99.
000060              11 MELDUNGS-BESCHREIBUNG.
000070                 13 MELDUNGS-NR   PICTURE 9(8).
000080                 13 SUB-VAR-1     PICTURE X(20).
000090                 13 SUB-VAR-2     PICTURE X(20).
000100              11 FEHL-BER-ALT     PICTURE X(45).
000110              11 FILLER           OCCURS 47 TIMES
000120                                  PICTURE X.
000130           09 SQL-BEREICH.
000140              11 VIEWNAME         PICTURE X(18).
000150              11 OPERATION        PICTURE XXX.
000160     88 KST-SELECT                VALUE 'SEL'.
000170     88 KST-INSERT                VALUE 'INS'.
000180     88 KST-UPDATE                VALUE 'UPD'.
000190     88 KST-DELETE                VALUE 'DEL'.
000200     88 KST-START                 VALUE 'ST '.
000210     88 KST-BACKWARD              VALUE 'BW '.
000220     88 KST-FORWARD               VALUE 'FW '.
000230     88 KST-OPEN                  VALUE 'OPE'.
000240     88 KST-CLOSE                 VALUE 'CLO'.
000250     88 KST-FETCH                 VALUE 'FET'.
000260              11 AUS-SQLCA.
000270                 13 SQLAID        PICTURE X(8).
000280                 13 SQLABC        PICTURE S9(9)  BINARY.
000290                 13 AUS-SQLCODE   PICTURE S9(9)  BINARY.
000300                 13 SQLERRM.
000310                    15 SQLERRML   PICTURE S9(4)  BINARY.
000320                    15 SQLERRMC   PICTURE X(70).
000330                 13 SQLERRP       PICTURE X(8).
000340                 13 SQLERRD       OCCURS 6 TIMES
000350                                  PICTURE S9(9)  BINARY.
000360                 13 SQLWARN.
000370                    15 SQLWARN0   PICTURE X.
000380                    15 SQLWARN1   PICTURE X.
000390                    15 SQLWARN2   PICTURE X.
000400                    15 SQLWARN3   PICTURE X.
000410                    15 SQLWARN4   PICTURE X.
000420                    15 SQLWARN5   PICTURE X.
000430                    15 SQLWARN6   PICTURE X.
000440                    15 SQLWARN7   PICTURE X.
000450                 13 SQLEXT.
000460                    15 SQLWARN8   PICTURE X.
000470                    15 SQLWARN9   PICTURE X.
000480                    15 SQLWARNA   PICTURE X.
000490                    15 AUS-SQLSTATE PICTURE X(5).
000500              11 FILLER           OCCURS 93 TIMES
000510                                  PICTURE X.
000520        07 ZL11-TEXT              PICTURE X(49).
000530        07 ZEILE-AB               PICTURE 9(4).
000540        07 ZEILE-BIS              PICTURE 9(4).
000550        07 ZEILE-VON              PICTURE 9(4).
000560        07 ZL12-TEXT              PICTURE X(15).
000570        07 ERG-DE-BEZ-MITTEL1     PICTURE X(10).
000580        07 ERG-DE-BEZ-MITTEL2     PICTURE X(10).
000590        07 ERG-DE-BEZ-MITTEL3     PICTURE X(10).
000600        07 ERG-DE-BEZ-MITTEL4     PICTURE X(10).
000610        07 DGOV1366               OCCURS 8 TIMES.
000620           09 ZL-TEXT             PICTURE X(15).
000630           09 ERG-DE-WERT1        PICTURE X(15).
000640           09 ERG-DE-WERT2        PICTURE X(15).
000650           09 ERG-DE-WERT3        PICTURE X(15).
000660           09 ERG-DE-WERT4        PICTURE X(15).
000670        07 DGOV0001.
000680           09 SICH-TABELLE-1      PICTURE X(424).
000690           09 SICH-TABELLE-2      PICTURE X(8000).
000700           09 SICH-ZW-FELDER      PICTURE X(238).
000710           09 SICH-DRMA1143.
000720              11 BUFFER-EINGABE-BEREICH.
000730                 13 PROGRAMMNAME  PICTURE X(8).
000740                 13 LAENGE        PICTURE S9(4)  BINARY.
000750                 13 MAXIMALE-ANZAHL
000760                                  PICTURE S9(4)  BINARY.
000770           09 SICH-DRMA1145.
000780              11 STEUER-BEREICH-BLAETTERN.
000790                 13 USERID        PICTURE X(8).
000800                 13 TRANSACTION-CODE
000810                                  PICTURE X(8).
000820                 13 OPERATION     PICTURE XX.
000830                 13 GEFORDERTE-ANZAHL
000840                                  PICTURE 9(4).
000850                 13 GELIEFERTE-ANZAHL
000860                                  PICTURE 9(4).
000870                 13 LETZTE-POSITION
000880                                  PICTURE 9(4).
000890                 13 GESAMT-ANZAHL PICTURE 9(4).
      *
      **************************************************************
