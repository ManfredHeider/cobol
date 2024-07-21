      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0031.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0031.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:58:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0031)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0031))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0031 TABLE
CCC004*    ( PCZ_BEZUGS_NR                  INTEGER NOT NULL,
CCC004*      PCZ_NR                         INTEGER NOT NULL,
CCC004*      PCZ_DAT_VON                    DATE NOT NULL,
CCC004*      PCZ_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KTY_PCZ_TYP                    SMALLINT NOT NULL,
CCC004*      KNR_PCZ_TYP                    SMALLINT NOT NULL,
CCC004*      PCZ_KENNZEICHEN                CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0031                           *
      ******************************************************************
       01  TBVB0031-GESAMT.
        05  NI-TBVB0031-TAB.
           10  NI-TBVB0031-ELE     PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-TBVB0031            REDEFINES   NI-TBVB0031-TAB.
           10 NI-PCZ-BEZUGS-NR     PIC S9(04)  COMP.
           10 NI-PCZ-NR            PIC S9(04)  COMP.
           10 NI-PCZ-DAT-VON       PIC S9(04)  COMP.
           10 NI-PCZ-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KTY-PCZ-TYP       PIC S9(04)  COMP.
           10 NI-KNR-PCZ-TYP       PIC S9(04)  COMP.
           10 NI-PCZ-KENNZEICHEN   PIC S9(04)  COMP.
        05  DCL-TBVB0031.
           10 PCZ-BEZUGS-NR        PIC S9(9) USAGE COMP.
           10 PCZ-NR               PIC S9(9) USAGE COMP.
           10 PCZ-DAT-VON          PIC X(10).
           10 PCZ-LAST-UPDATE      PIC X(26).
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KTY-PCZ-TYP          PIC S9(4) USAGE COMP.
           10 KNR-PCZ-TYP          PIC S9(4) USAGE COMP.
           10 PCZ-KENNZEICHEN      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
