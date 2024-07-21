      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3171.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3171.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 18:09:14   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 17:12:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA3163 - DELETE
      *
      *     SOURCE-NAME      :  DRMA3171
      *     DMR-NAME         :  DG-OV3171
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  28.03.06/10:51
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3171.
              07 STATUS-SQL-BEREICH.
                 09 STATUS-BEREICH.
                    11 PROGRAMMNAME     PICTURE X(8).
                    11 RC               PICTURE 99.
                    11 MELDUNGS-BESCHREIBUNG.
                       13 MELDUNGS-NR   PICTURE 9(8).
                       13 SUB-VAR-1     PICTURE X(20).
                       13 SUB-VAR-2     PICTURE X(20).
                    11 FEHL-BER-ALT     PICTURE X(45).
                    11 FILLER           OCCURS 47 TIMES
                                        PICTURE X.
                 09 SQL-BEREICH.
                    11 VIEWNAME         PICTURE X(18).
                    11 OPERATION        PICTURE XXX.
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
                       13 SQLAID        PICTURE X(8).
                       13 SQLABC        PICTURE S9(9)  BINARY.
                       13 AUS-SQLCODE   PICTURE S9(9)  BINARY.
                       13 SQLERRM.
                          15 SQLERRML   PICTURE S9(4)  BINARY.
                          15 SQLERRMC   PICTURE X(70).
                       13 SQLERRP       PICTURE X(8).
                       13 SQLERRD       OCCURS 6 TIMES
                                        PICTURE S9(9)  BINARY.
                       13 SQLWARN.
                          15 SQLWARN0   PICTURE X.
                          15 SQLWARN1   PICTURE X.
                          15 SQLWARN2   PICTURE X.
                          15 SQLWARN3   PICTURE X.
                          15 SQLWARN4   PICTURE X.
                          15 SQLWARN5   PICTURE X.
                          15 SQLWARN6   PICTURE X.
                          15 SQLWARN7   PICTURE X.
                       13 SQLEXT.
                          15 SQLWARN8   PICTURE X.
                          15 SQLWARN9   PICTURE X.
                          15 SQLWARNA   PICTURE X.
                          15 AUS-SQLSTATE PICTURE X(5).
                    11 FILLER           OCCURS 93 TIMES
                                        PICTURE X.
      *
      **************************************************************
