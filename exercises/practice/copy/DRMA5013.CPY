      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5013.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5013.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 15:53:36   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Oct 15 2013 11:07:20   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jun 24 2013 13:17:12   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!24.06.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5013.
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
                          15 AUS-SQLSTATE
                                        PIC X(5).
                    11 FILLER           OCCURS 93 TIMES
                                        PIC X.
              07 DRMA5015.
                 09 LRECL               pic X(5).
                 09 PRI                 pic X(8).
                 09 SEC                 pic X(8).
                 09 AVG                 pic X.
                 09 PLA-UNIT            pic X.
                 09 PLA-TAPE            pic X.
                 09                     pic X(56).

      *
      **************************************************************

