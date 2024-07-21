      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4344.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA4344.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 18:17:18   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 17:29:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA4342 - Listen-1
      *     RDz
      *     SOURCE-NAME      :  DRMA4344
      *     DMR-NAME         :  DG-OV4344
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  02.12.2011/09:00:42
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4344.
              07 STATUS-SQL-BEREICH.
                 09 STATUS-BEREICH.
                    11 PROGRAMMNAME     PIC X(8).
                    11 RC               PIC 99.
                    11 MELDUNGS-BESCHREIBUNG.
                       13 MELDUNGS-NR   PIC 9(8).
                       13 SUB-VAR-1     PIC X(20).
                       13 SUB-VAR-2     PIC X(20).
                    11 FEHL-BER-ALT     PIC X(45).
                    11 FILLER           OCCURS 47 TIMES
                                        PIC X.
                 09 SQL-BEREICH.
                    11 VIEWNAME         PIC X(18).
                    11 OPERATION        PIC XXX.
           88 KST-SELECT                VALUE 'SEL'.
           88 KST-INSERT                VALUE 'INS'.
           88 KST-UPDATE                VALUE 'UPD'.
           88 KST-DELETE                VALUE 'DEL'.
           88 KST-START                 VALUE 'ST '.
           88 KST-BACKWARD              VALUE 'BW '.
           88 KST-FORWARD               VALUE 'FW '.
           88 KST-OPEN                  VALUE 'OPE'.
           88 KST-CLOSE                 VALUE 'CLO'.
           88 KST-FETCH                 VALUE 'FET'.
                    11 AUS-SQLCA.
                       13 SQLAID        PIC X(8).
                       13 SQLABC        PIC S9(9)      BINARY.
                       13 AUS-SQLCODE   PIC S9(9)      BINARY.
                       13 SQLERRM.
                          15 SQLERRML   PIC S9(4)      BINARY.
                          15 SQLERRMC   PIC X(70).
                       13 SQLERRP       PIC X(8).
                       13 SQLERRD       OCCURS 6 TIMES
                                        PIC S9(9)      BINARY.
                       13 SQLWARN.
                          15 SQLWARN0   PIC X.
                          15 SQLWARN1   PIC X.
                          15 SQLWARN2   PIC X.
                          15 SQLWARN3   PIC X.
                          15 SQLWARN4   PIC X.
                          15 SQLWARN5   PIC X.
                          15 SQLWARN6   PIC X.
                          15 SQLWARN7   PIC X.
                       13 SQLEXT.
                          15 SQLWARN8   PIC X.
                          15 SQLWARN9   PIC X.
                          15 SQLWARNA   PIC X.
                          15 AUS-SQLSTATE PIC X(5).
                    11 FILLER           OCCURS 93 TIMES
                                        PIC X.
              07 ANALYSEDATENVB.
                 09 ANAL-LFDNR          PIC S9(4)      BINARY.
                 09 SEL-DE-VG-WERT      PIC S9(4)      BINARY.
                 09 SEL-DE-DAT-WERT     PIC X(10).
                 09 SEL-DE-VBNR-WERT    PIC S9(9)      BINARY.
                 09 SEL-DE-DIR-WERT     PIC X(4).
                 09 SEL-DE-STUFE-WERT   PIC X(4).
                 09 DGOV1954.
                    11 SEL-DE-1-WERT    PIC X(15).
                    11 SEL-DE-2-WERT    PIC X(15).
                    11 SEL-DE-3-WERT    PIC X(15).
                    11 SEL-DE-4-WERT    PIC X(15).
                 09 DGOV1955 REDEFINES DGOV1954.
                    11 SEL-DE-WERT      OCCURS 4 TIMES
                                        PIC X(15).
                 09 DGOV1956.
                    11 ERG-DE-0-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-1-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-2-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-3-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-4-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-5-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-6-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-7-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-8-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-9-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                 09 DGOV1957 REDEFINES DGOV1956.
                    11 ERG-DE-WERT      OCCURS 10 TIMES
                                        PIC S9(15)V99  PACKED-DECIMAL.
                 09 AENDERUNGSSYSZEIT   PIC X(26).
                 09 AENDERUNGSKENNUNG   PIC X(8).
      *
      **************************************************************
