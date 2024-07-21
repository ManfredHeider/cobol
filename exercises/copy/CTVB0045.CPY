      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0045.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0045.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:01:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0045)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0045))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0045 TABLE
CCC004*    ( ED_ST_NR                       INTEGER NOT NULL,
CCC004*      EDT_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      EDT_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      EDT_DAT_BIS                    DATE NOT NULL,
CCC004*      KTY_EDT_AUS_GRND               SMALLINT NOT NULL,
CCC004*      KNR_EDT_AUS_GRND               SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0045                           *
      ******************************************************************
       01  TBVB0045-GESAMT.
        05  NI-TBVB0045-TAB.
           10  NI-TBVB0045-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-TBVB0045            REDEFINES   NI-TBVB0045-TAB.
           10 NI-ED-ST-NR          PIC S9(04)  COMP.
           10 NI-EDT-DAT-VON       PIC S9(04)  COMP.
           10 NI-EDT-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-EDT-DAT-BIS       PIC S9(04)  COMP.
           10 NI-KTY-EDT-AUS-GRND  PIC S9(04)  COMP.
           10 NI-KNR-EDT-AUS-GRND  PIC S9(04)  COMP.
        05  DCL-TBVB0045.
           10 ED-ST-NR             PIC S9(9) USAGE COMP.
           10 EDT-DAT-VON          PIC X(26).
           10 EDT-LAST-UPDATE      PIC X(26).
           10 EDT-DAT-BIS          PIC X(10).
           10 KTY-EDT-AUS-GRND     PIC S9(4) USAGE COMP.
           10 KNR-EDT-AUS-GRND     PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
