      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:16:50   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0027)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0027))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0027 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      FAPZ_DAT_VON                   DATE NOT NULL,
CCC004*      FAPZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      FAPZ_VERS_NR                   DECIMAL(15, 0) NOT NULL,
CCC004*      FAPZ_STF                       CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0027                           *
      ******************************************************************
       01  VIVB0027-GESAMT.
        05  NI-VIVB0027-TAB.
           10  NI-VIVB0027-ELE     PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  NI-VIVB0027            REDEFINES   NI-VIVB0027-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-FAPZ-DAT-VON      PIC S9(04)  COMP.
           10 NI-FAPZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-FAPZ-VERS-NR      PIC S9(04)  COMP.
           10 NI-FAPZ-STF          PIC S9(04)  COMP.
        05  DCL-VIVB0027.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 FAPZ-DAT-VON         PIC X(10).
           10 FAPZ-LAST-UPDATE     PIC X(26).
           10 FAPZ-VERS-NR         PIC S999999999999999V USAGE COMP-3.
           10 FAPZ-STF             PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
