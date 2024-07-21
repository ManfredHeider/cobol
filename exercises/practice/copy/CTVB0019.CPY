      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:55:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0019)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0019))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0019 TABLE
CCC004*    ( DL_BEZUGS_NR                   INTEGER NOT NULL,
CCC004*      DIR_NR                         SMALLINT NOT NULL,
CCC004*      DL_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      DL_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      DL_DAT_BIS                     DATE NOT NULL,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0019                           *
      ******************************************************************
       01  TBVB0019-GESAMT.
        05  NI-TBVB0019-TAB.
           10  NI-TBVB0019-ELE     PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-TBVB0019            REDEFINES   NI-TBVB0019-TAB.
           10 NI-DL-BEZUGS-NR      PIC S9(04)  COMP.
           10 NI-DIR-NR            PIC S9(04)  COMP.
           10 NI-DL-DAT-VON        PIC S9(04)  COMP.
           10 NI-DL-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-DL-DAT-BIS        PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
        05  DCL-TBVB0019.
           10 DL-BEZUGS-NR         PIC S9(9) USAGE COMP.
           10 DIR-NR               PIC S9(4) USAGE COMP.
           10 DL-DAT-VON           PIC X(26).
           10 DL-LAST-UPDATE       PIC X(26).
           10 DL-DAT-BIS           PIC X(10).
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
