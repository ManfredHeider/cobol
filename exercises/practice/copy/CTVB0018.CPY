      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:55:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0018)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0018))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0018 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      DIR_NR                         SMALLINT NOT NULL,
CCC004*      DZ_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      DZ_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      DZ_DAT_BIS                     DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0018                           *
      ******************************************************************
       01  TBVB0018-GESAMT.
        05  NI-TBVB0018-TAB.
           10  NI-TBVB0018-ELE     PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  NI-TBVB0018            REDEFINES   NI-TBVB0018-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-DIR-NR            PIC S9(04)  COMP.
           10 NI-DZ-DAT-VON        PIC S9(04)  COMP.
           10 NI-DZ-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-DZ-DAT-BIS        PIC S9(04)  COMP.
        05  DCL-TBVB0018.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 DIR-NR               PIC S9(4) USAGE COMP.
           10 DZ-DAT-VON           PIC X(26).
           10 DZ-LAST-UPDATE       PIC X(26).
           10 DZ-DAT-BIS           PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
