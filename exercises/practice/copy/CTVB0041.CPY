      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0041.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0041.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:00:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( TBVB0041)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0041))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TBVB0041 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      DIR_NR                         SMALLINT NOT NULL,
CCC004*      SPLZ_BEREICH                   CHAR(20) NOT NULL,
CCC004*      SPLZ_LAST_UPDATE               TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBVB0041                          *
      ******************************************************************
       01  TBVB0041-GESAMT.
        05  NI-TBVB0041-TAB.
           10  NI-TBVB0041-ELE     PIC S9(04)  COMP   OCCURS  4  TIMES.
        05  NI-TBVB0041            REDEFINES   NI-TBVB0041-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-DIR-NR            PIC S9(04)  COMP.
           10 NI-SPLZ-BEREICH      PIC S9(04)  COMP.
           10 NI-SPLZ-LAST-UPDATE  PIC S9(04)  COMP.
        05  DCL-TBVB0041.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 DIR-NR               PIC S9(4) USAGE COMP.
           10 SPLZ-BEREICH         PIC X(20).
           10 SPLZ-LAST-UPDATE     PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
