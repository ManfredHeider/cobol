      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0029.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0029.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:17:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0029)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0029))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0029 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      KTY_SLZ_TYP                    SMALLINT NOT NULL,
CCC004*      KNR_SLZ_TYP                    SMALLINT NOT NULL,
CCC004*      SLZ_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      SLZ_DAT_VON                    DATE NOT NULL,
CCC004*      SLZ_BESCHREIBUNG               CHAR(10) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0029                           *
      ******************************************************************
       01  VIVB0029-GESAMT.
        05  NI-VIVB0029-TAB.
           10  NI-VIVB0029-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-VIVB0029            REDEFINES   NI-VIVB0029-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-KTY-SLZ-TYP       PIC S9(04)  COMP.
           10 NI-KNR-SLZ-TYP       PIC S9(04)  COMP.
           10 NI-SLZ-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-SLZ-DAT-VON       PIC S9(04)  COMP.
           10 NI-SLZ-BESCHREIBUNG  PIC S9(04)  COMP.
        05  DCL-VIVB0029.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KTY-SLZ-TYP          PIC S9(4) USAGE COMP.
           10 KNR-SLZ-TYP          PIC S9(4) USAGE COMP.
           10 SLZ-LAST-UPDATE      PIC X(26).
           10 SLZ-DAT-VON          PIC X(10).
           10 SLZ-BESCHREIBUNG     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
