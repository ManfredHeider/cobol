      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1650.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA1650.CPv  $
      *
      *   Rev 2.3   Jul 23 2013 19:18:28   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.2   Jul 23 2013 17:48:18   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 10:17:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1648 - SELECT
      *
      *     SOURCE-NAME      :  DRMA1650
      *     DMR-NAME         :  DG-OV1650
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/15:49
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1650.
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
000520        07 AUSWERTUNGVB.
000530           09 AUSW-LFDNR          PICTURE S9(4)  BINARY.
000540           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000550           09 GUELT-AB            PICTURE X(10).
000560           09 GUELT-BIS           PICTURE X(10).
000570           09 SEL-DE-VG-LFDNR     PICTURE S9(4)  BINARY.
000580           09 SEL-DE-DAT-LFDNR    PICTURE S9(4)  BINARY.
000590           09 SEL-DE-VBNR-LFDNR   PICTURE S9(4)  BINARY.
000600           09 SEL-DE-DIR-LFDNR    PICTURE S9(4)  BINARY.
000610           09 SEL-DE-STUFE-LFDNR  PICTURE S9(4)  BINARY.
000620           09 DRMA1950.
000630              11 SEL-DE-1-LFDNR   PICTURE S9(4)  BINARY.
000640              11 SEL-DE-2-LFDNR   PICTURE S9(4)  BINARY.
000650              11 SEL-DE-3-LFDNR   PICTURE S9(4)  BINARY.
000660              11 SEL-DE-4-LFDNR   PICTURE S9(4)  BINARY.
000670           09 DRMA1951 REDEFINES DRMA1950.
000680              11 SEL-DE-LFDNR     OCCURS 4 TIMES
000690                                  PICTURE S9(4)  BINARY.
000700           09 DRMA1952.
000710              11 ERG-DE-0-LFDNR   PICTURE S9(4)  BINARY.
000720              11 ERG-DE-1-LFDNR   PICTURE S9(4)  BINARY.
000730              11 ERG-DE-2-LFDNR   PICTURE S9(4)  BINARY.
000740              11 ERG-DE-3-LFDNR   PICTURE S9(4)  BINARY.
000750              11 ERG-DE-4-LFDNR   PICTURE S9(4)  BINARY.
000760              11 ERG-DE-5-LFDNR   PICTURE S9(4)  BINARY.
000770              11 ERG-DE-6-LFDNR   PICTURE S9(4)  BINARY.
000780              11 ERG-DE-7-LFDNR   PICTURE S9(4)  BINARY.
000790              11 ERG-DE-8-LFDNR   PICTURE S9(4)  BINARY.
000800              11 ERG-DE-9-LFDNR   PICTURE S9(4)  BINARY.
000810              11 ERG-DE-10-LFDNR  PICTURE S9(4)  BINARY.
000820              11 ERG-DE-11-LFDNR  PICTURE S9(4)  BINARY.
000830              11 ERG-DE-12-LFDNR  PICTURE S9(4)  BINARY.
000840              11 ERG-DE-13-LFDNR  PICTURE S9(4)  BINARY.
000850              11 ERG-DE-14-LFDNR  PICTURE S9(4)  BINARY.
000860              11 ERG-DE-15-LFDNR  PICTURE S9(4)  BINARY.
000870              11 ERG-DE-16-LFDNR  PICTURE S9(4)  BINARY.
000880              11 ERG-DE-17-LFDNR  PICTURE S9(4)  BINARY.
000890              11 ERG-DE-18-LFDNR  PICTURE S9(4)  BINARY.
000900              11 ERG-DE-19-LFDNR  PICTURE S9(4)  BINARY.
000910           09 DRMA1953 REDEFINES DRMA1952.
000920              11 ERG-DE-LFDNR     OCCURS 20 TIMES
000930                                  PICTURE S9(4)  BINARY.
000940           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000950           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************
