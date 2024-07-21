      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0517.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0517.CPv  $
      *
      *   Rev 2.2   Jul 24 2013 16:38:14   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jun 28 2013 15:01:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0517.
000020        07 TIMESTAMP15            PICTURE S9(15) PACKED-DECIMAL.
000030        07 STATUS-SQL-BEREICH.
000040           09 STATUS-BEREICH.
000050              11 PROGRAMMNAME     PICTURE X(8).
000060              11 RC               PICTURE 99.
000070              11 MELDUNGS-BESCHREIBUNG.
000080                 13 MELDUNGS-NR   PICTURE 9(8).
000090                 13 SUB-VAR-1     PICTURE X(20).
000100                 13 SUB-VAR-2     PICTURE X(20).
000110              11 FEHL-BER-ALT     PICTURE X(45).
000120              11 FILLER           OCCURS 47 TIMES
000130                                  PICTURE X.
000140           09 SQL-BEREICH.
000150              11 VIEWNAME         PICTURE X(18).
000160              11 OPERATION        PICTURE XXX.
000170     88 KST-SELECT                VALUE 'SEL'.
000180     88 KST-INSERT                VALUE 'INS'.
000190     88 KST-UPDATE                VALUE 'UPD'.
000200     88 KST-DELETE                VALUE 'DEL'.
000210     88 KST-START                 VALUE 'ST '.
000220     88 KST-BACKWARD              VALUE 'BW '.
000230     88 KST-FORWARD               VALUE 'FW '.
000240     88 KST-OPEN                  VALUE 'OPE'.
000250     88 KST-CLOSE                 VALUE 'CLO'.
000260     88 KST-FETCH                 VALUE 'FET'.
000270              11 AUS-SQLCA.
000280                 13 SQLAID        PICTURE X(8).
000290                 13 SQLABC        PICTURE S9(9)  BINARY.
000300                 13 AUS-SQLCODE   PICTURE S9(9)  BINARY.
000310                 13 SQLERRM.
000320                    15 SQLERRML   PICTURE S9(4)  BINARY.
000330                    15 SQLERRMC   PICTURE X(70).
000340                 13 SQLERRP       PICTURE X(8).
000350                 13 SQLERRD       OCCURS 6 TIMES
000360                                  PICTURE S9(9)  BINARY.
000370                 13 SQLWARN.
000380                    15 SQLWARN0   PICTURE X.
000390                    15 SQLWARN1   PICTURE X.
000400                    15 SQLWARN2   PICTURE X.
000410                    15 SQLWARN3   PICTURE X.
000420                    15 SQLWARN4   PICTURE X.
000430                    15 SQLWARN5   PICTURE X.
000440                    15 SQLWARN6   PICTURE X.
000450                    15 SQLWARN7   PICTURE X.
000460                 13 SQLEXT.
000470                    15 SQLWARN8   PICTURE X.
000480                    15 SQLWARN9   PICTURE X.
000490                    15 SQLWARNA   PICTURE X.
000500                    15 AUS-SQLSTATE PICTURE X(5).
000510              11 FILLER           OCCURS 93 TIMES
000520                                  PICTURE X.
