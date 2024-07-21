      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0025.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0025.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:56:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0025)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0025))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0025 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      AFBZ_MONAT                     DATE NOT NULL,
CCC004*      AFBZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      AFBZ_BETRAG                    DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0025                           *
      ******************************************************************
       01  TBVB0025-GESAMT.
        05  NI-TBVB0025-TAB.
           10  NI-TBVB0025-ELE     PIC S9(04)  COMP   OCCURS  4  TIMES.
        05  NI-TBVB0025            REDEFINES   NI-TBVB0025-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-AFBZ-MONAT        PIC S9(04)  COMP.
           10 NI-AFBZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-AFBZ-BETRAG       PIC S9(04)  COMP.
        05  DCL-TBVB0025.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 AFBZ-MONAT           PIC X(10).
           10 AFBZ-LAST-UPDATE     PIC X(26).
           10 AFBZ-BETRAG          PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
