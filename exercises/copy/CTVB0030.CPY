      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:57:50   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *      VERSION   1    22.06.94 18:04:32
      ******************************************************************
      * DCLGEN TABLE(TBVB0030)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0030))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0030 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      ZV_NR                          CHAR(1) NOT NULL,
CCC004*      ZVZ_DAT_VON                    DATE NOT NULL,
CCC004*      ZVZ_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      ZVZ_DAT_BIS                    DATE NOT NULL,
CCC004*      ZVZ_KUE_DAT                    DATE,
CCC004*      ZVZ_DAT_BIS_R                  DATE NOT NULL,
CCC004*      ZVZ_VERL                       CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0030                           *
      ******************************************************************
       01  TBVB0030-GESAMT.
        05  NI-TBVB0030-TAB.
           10  NI-TBVB0030-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-TBVB0030            REDEFINES   NI-TBVB0030-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-ZV-NR             PIC S9(04)  COMP.
           10 NI-ZVZ-DAT-VON       PIC S9(04)  COMP.
           10 NI-ZVZ-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-ZVZ-DAT-BIS       PIC S9(04)  COMP.
           10 NI-ZVZ-KUE-DAT       PIC S9(04)  COMP.
           10 NI-ZVZ-DAT-BIS-R     PIC S9(04)  COMP.
           10 NI-ZVZ-VERL          PIC S9(04)  COMP.
        05  DCL-TBVB0030.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 ZV-NR                PIC X(1).
           10 ZVZ-DAT-VON          PIC X(10).
           10 ZVZ-LAST-UPDATE      PIC X(26).
           10 ZVZ-DAT-BIS          PIC X(10).
           10 ZVZ-KUE-DAT          PIC X(10).
           10 ZVZ-DAT-BIS-R        PIC X(10).
           10 ZVZ-VERL             PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
