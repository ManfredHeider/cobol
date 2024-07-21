      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0006.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0006.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 17:42:20   t13ac19
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jun 28 2013 16:00:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1799 - Listen-2
      *
      *     SOURCE-NAME      :  DRMA0006
      *     DMR-NAME         :  DG-OV0006
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  17.04.01/07:20
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA0006.
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
000520        07 ANALYSEDATEN-7.
000530           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000540           09 SEL-DE-DAT-WERT     PICTURE X(10).
000550           09 DGOV1958.
000560              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000570                                                 PACKED-DECIMAL.
000580              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000590                                                 PACKED-DECIMAL.
000600              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000610                                                 PACKED-DECIMAL.
000620              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000630                                                 PACKED-DECIMAL.
000640              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000650                                                 PACKED-DECIMAL.
000660              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000670                                                 PACKED-DECIMAL.
000680              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000690                                                 PACKED-DECIMAL.
000700              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000710                                                 PACKED-DECIMAL.
000720              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000730                                                 PACKED-DECIMAL.
000740              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000750                                                 PACKED-DECIMAL.
000760              11 ERG-DE-10-WERT   PICTURE S9(15)V99
000770                                                 PACKED-DECIMAL.
000780              11 ERG-DE-11-WERT   PICTURE S9(15)V99
000790                                                 PACKED-DECIMAL.
000800              11 ERG-DE-12-WERT   PICTURE S9(15)V99
000810                                                 PACKED-DECIMAL.
000820              11 ERG-DE-13-WERT   PICTURE S9(15)V99
000830                                                 PACKED-DECIMAL.
000840              11 ERG-DE-14-WERT   PICTURE S9(15)V99
000850                                                 PACKED-DECIMAL.
000860              11 ERG-DE-15-WERT   PICTURE S9(15)V99
000870                                                 PACKED-DECIMAL.
000880              11 ERG-DE-16-WERT   PICTURE S9(15)V99
000890                                                 PACKED-DECIMAL.
000900              11 ERG-DE-17-WERT   PICTURE S9(15)V99
000910                                                 PACKED-DECIMAL.
000920              11 ERG-DE-18-WERT   PICTURE S9(15)V99
000930                                                 PACKED-DECIMAL.
000940              11 ERG-DE-19-WERT   PICTURE S9(15)V99
000950                                                 PACKED-DECIMAL.
000960           09 DGOV1959 REDEFINES DGOV1958.
000970              11 ERG-DE-WERT      OCCURS 20 TIMES
000980                                  PICTURE S9(15)V99
000990                                                 PACKED-DECIMAL.
001000           09 SEL-DE-DAT-WERT1    PICTURE X(10).
001010           09 SEL-DE-DAT-WERT2    PICTURE X(10).
001020           09 SEL-DE-DAT-WERT3    PICTURE X(10).
001030           09 SEL-DE-DAT-WERT4    PICTURE X(10).
001040           09 SEL-DE-DAT-WERT5    PICTURE X(10).
001050           09 SEL-DE-DAT-WERT6    PICTURE X(10).
001060           09 SEL-DE-DAT-WERT7    PICTURE X(10).
001070           09 SEL-DE-DAT-WERT8    PICTURE X(10).
001080           09 SEL-DE-DAT-WERT9    PICTURE X(10).
001090           09 SEL-DE-DAT-WERT10   PICTURE X(10).
001100           09 SEL-DE-DAT-WERT11   PICTURE X(10).
001110           09 SEL-DE-DAT-WERT12   PICTURE X(10).
001120           09 SEL-DE-DAT-WERT13   PICTURE X(10).
001130           09 SEL-DE-DAT-WERT14   PICTURE X(10).
001140           09 SEL-DE-DAT-WERT15   PICTURE X(10).
001150           09 SEL-DE-DAT-WERT16   PICTURE X(10).
001160           09 SEL-DE-DAT-WERT17   PICTURE X(10).
001170           09 SEL-DE-DAT-WERT18   PICTURE X(10).
001180           09 SEL-DE-DAT-WERT19   PICTURE X(10).
001190           09 SEL-DE-DAT-WERT20   PICTURE X(10).
      *
      **************************************************************
