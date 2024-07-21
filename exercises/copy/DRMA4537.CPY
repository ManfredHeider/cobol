      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4537.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA4537.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 18:18:26   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 17:31:26   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich PMO4524
      *     RDz
      *     SOURCE-NAME      :  DRMA4537
      *     DMR-NAME         :  DG-OV4537
      *
      *     AUTOR            :  CONSOIR,ANNA E.
      *     GENERIERUNG      :  29.06.2012/11:41:17
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DG-OV4537.
              07 DRAT0561.
                 09 STATUS-SQL-BEREICH.
                    11 STATUS-BEREICH.
                       13 PROGRAMMNAME  PIC X(8).
                       13 RC            PIC 99.
                       13 MELDUNGS-BESCHREIBUNG.
                          15 MELDUNGS-NR
                                        PIC 9(8).
                          15 SUB-VAR-1  PIC X(20).
                          15 SUB-VAR-2  PIC X(20).
                       13 FEHL-BER-ALT  PIC X(45).
                       13 FILLER        OCCURS 47 TIMES
                                        PIC X.
                    11 SQL-BEREICH.
                       13 VIEWNAME      PIC X(18).
                       13 OPERATION     PIC XXX.
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
                       13 AUS-SQLCA.
                          15 SQLAID     PIC X(8).
                          15 SQLABC     PIC S9(9)      BINARY.
                          15 AUS-SQLCODE PIC S9(9)      BINARY.
                          15 SQLERRM.
                             17 SQLERRML
                                        PIC S9(4)      BINARY.
                             17 SQLERRMC
                                        PIC X(70).
                          15 SQLERRP    PIC X(8).
                          15 SQLERRD    OCCURS 6 TIMES
                                        PIC S9(9)      BINARY.
                          15 SQLWARN.
                             17 SQLWARN0
                                        PIC X.
                             17 SQLWARN1
                                        PIC X.
                             17 SQLWARN2
                                        PIC X.
                             17 SQLWARN3
                                        PIC X.
                             17 SQLWARN4
                                        PIC X.
                             17 SQLWARN5
                                        PIC X.
                             17 SQLWARN6
                                        PIC X.
                             17 SQLWARN7
                                        PIC X.
                          15 SQLEXT.
                             17 SQLWARN8
                                        PIC X.
                             17 SQLWARN9
                                        PIC X.
                             17 SQLWARNA
                                        PIC X.
                             17 AUS-SQLSTATE
                                        PIC X(5).
                       13 FILLER        OCCURS 93 TIMES
                                        PIC X.
              07 VG                     PIC 9(4).
      *
      **************************************************************
