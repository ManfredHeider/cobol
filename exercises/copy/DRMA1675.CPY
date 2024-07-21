      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1675.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1675.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 18:56:52   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 10:21:44   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1388 - Steuerung Listen-ALLG
      *
      *     SOURCE-NAME      :  DRMA1675
      *     DMR-NAME         :  DG-OV1675
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/16:23
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1675.
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
000520        07 ANALDATEN-ALLG.
000530           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000540           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000550           09 SEL-DE-DAT-WERT     PICTURE X(10).
000560           09 DGOV1954.
000570              11 SEL-DE-1-WERT    PICTURE X(15).
000580              11 SEL-DE-2-WERT    PICTURE X(15).
000590              11 SEL-DE-3-WERT    PICTURE X(15).
000600              11 SEL-DE-4-WERT    PICTURE X(15).
000610           09 DGOV1955 REDEFINES DGOV1954.
000620              11 SEL-DE-WERT      OCCURS 4 TIMES
000630                                  PICTURE X(15).
000640           09 DGOV1956.
000650              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000660                                                 PACKED-DECIMAL.
000670              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000680                                                 PACKED-DECIMAL.
000690              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000700                                                 PACKED-DECIMAL.
000710              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000720                                                 PACKED-DECIMAL.
000730              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000740                                                 PACKED-DECIMAL.
000750              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000760                                                 PACKED-DECIMAL.
000770              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000780                                                 PACKED-DECIMAL.
000790              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000800                                                 PACKED-DECIMAL.
000810              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000820                                                 PACKED-DECIMAL.
000830              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000840                                                 PACKED-DECIMAL.
000850           09 DGOV1957 REDEFINES DGOV1956.
000860              11 ERG-DE-WERT      OCCURS 10 TIMES
000870                                  PICTURE S9(15)V99
000880                                                 PACKED-DECIMAL.
000890           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000900           09 AENDERUNGSKENNUNG   PICTURE X(8).
000910        07 VGLDATEN-ALLG.
000920           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000930           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000940           09 SEL-DE-DAT-WERT     PICTURE X(10).
000950           09 DGOV1954.
000960              11 SEL-DE-1-WERT    PICTURE X(15).
000970              11 SEL-DE-2-WERT    PICTURE X(15).
000980              11 SEL-DE-3-WERT    PICTURE X(15).
000990              11 SEL-DE-4-WERT    PICTURE X(15).
001000           09 DGOV1955 REDEFINES DGOV1954.
001010              11 SEL-DE-WERT      OCCURS 4 TIMES
001020                                  PICTURE X(15).
001030           09 DGOV1956.
001040              11 ERG-DE-0-WERT    PICTURE S9(15)V99
001050                                                 PACKED-DECIMAL.
001060              11 ERG-DE-1-WERT    PICTURE S9(15)V99
001070                                                 PACKED-DECIMAL.
001080              11 ERG-DE-2-WERT    PICTURE S9(15)V99
001090                                                 PACKED-DECIMAL.
001100              11 ERG-DE-3-WERT    PICTURE S9(15)V99
001110                                                 PACKED-DECIMAL.
001120              11 ERG-DE-4-WERT    PICTURE S9(15)V99
001130                                                 PACKED-DECIMAL.
001140              11 ERG-DE-5-WERT    PICTURE S9(15)V99
001150                                                 PACKED-DECIMAL.
001160              11 ERG-DE-6-WERT    PICTURE S9(15)V99
001170                                                 PACKED-DECIMAL.
001180              11 ERG-DE-7-WERT    PICTURE S9(15)V99
001190                                                 PACKED-DECIMAL.
001200              11 ERG-DE-8-WERT    PICTURE S9(15)V99
001210                                                 PACKED-DECIMAL.
001220              11 ERG-DE-9-WERT    PICTURE S9(15)V99
001230                                                 PACKED-DECIMAL.
001240           09 DGOV1957 REDEFINES DGOV1956.
001250              11 ERG-DE-WERT      OCCURS 10 TIMES
001260                                  PICTURE S9(15)V99
001270                                                 PACKED-DECIMAL.
001280           09 AENDERUNGSSYSZEIT   PICTURE X(26).
001290           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************
