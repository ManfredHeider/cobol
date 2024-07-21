      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1490.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1490.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 17:56:30   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 09:03:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1483 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1490
      *     DMR-NAME         :  DG-OV1490
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  29.05.00/14:18
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1490.
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
      *
      **************************************************************
