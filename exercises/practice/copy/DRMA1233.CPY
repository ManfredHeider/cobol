      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1233.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1233.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 17:46:14   t13ac19
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jun 28 2013 16:19:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1231 - Listen-1
      *
      *     SOURCE-NAME      :  DRMA1233
      *     DMR-NAME         :  DG-OV1233
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  23.03.01/13:57
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1233.
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
000520        07 ANALYSEDATENVB-51.
000530           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000540           09 SEL-DE-DAT-WERT     PICTURE X(10).
000550           09 SEL-DE-VBNR-WERT    PICTURE S9(9)  BINARY.
000560           09 SEL-DE-DIR-WERT     PICTURE X(4).
000570           09 SEL-DE-STUFE-WERT   PICTURE X(4).
000580           09 DGOV1954.
000590              11 SEL-DE-1-WERT    PICTURE X(15).
000600              11 SEL-DE-2-WERT    PICTURE X(15).
000610              11 SEL-DE-3-WERT    PICTURE X(15).
000620              11 SEL-DE-4-WERT    PICTURE X(15).
000630           09 DGOV1955 REDEFINES DGOV1954.
000640              11 SEL-DE-WERT      OCCURS 4 TIMES
000650                                  PICTURE X(15).
000660           09 DGOV1958.
000670              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000680                                                 PACKED-DECIMAL.
000690              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000700                                                 PACKED-DECIMAL.
000710              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000720                                                 PACKED-DECIMAL.
000730              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000740                                                 PACKED-DECIMAL.
000750              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000760                                                 PACKED-DECIMAL.
000770              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000780                                                 PACKED-DECIMAL.
000790              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000800                                                 PACKED-DECIMAL.
000810              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000820                                                 PACKED-DECIMAL.
000830              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000840                                                 PACKED-DECIMAL.
000850              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000860                                                 PACKED-DECIMAL.
000870              11 ERG-DE-10-WERT   PICTURE S9(15)V99
000880                                                 PACKED-DECIMAL.
000890              11 ERG-DE-11-WERT   PICTURE S9(15)V99
000900                                                 PACKED-DECIMAL.
000910              11 ERG-DE-12-WERT   PICTURE S9(15)V99
000920                                                 PACKED-DECIMAL.
000930              11 ERG-DE-13-WERT   PICTURE S9(15)V99
000940                                                 PACKED-DECIMAL.
000950              11 ERG-DE-14-WERT   PICTURE S9(15)V99
000960                                                 PACKED-DECIMAL.
000970              11 ERG-DE-15-WERT   PICTURE S9(15)V99
000980                                                 PACKED-DECIMAL.
000990              11 ERG-DE-16-WERT   PICTURE S9(15)V99
001000                                                 PACKED-DECIMAL.
001010              11 ERG-DE-17-WERT   PICTURE S9(15)V99
001020                                                 PACKED-DECIMAL.
001030              11 ERG-DE-18-WERT   PICTURE S9(15)V99
001040                                                 PACKED-DECIMAL.
001050              11 ERG-DE-19-WERT   PICTURE S9(15)V99
001060                                                 PACKED-DECIMAL.
001070           09 DGOV1959 REDEFINES DGOV1958.
001080              11 ERG-DE-WERT      OCCURS 20 TIMES
001090                                  PICTURE S9(15)V99
001100                                                 PACKED-DECIMAL.
      *
      **************************************************************
