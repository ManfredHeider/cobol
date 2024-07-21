      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:11:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0001)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0001))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0001 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      ST_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      ST_PRUEFZIFFER                 CHAR(1) NOT NULL,
CCC004*      ST_FREI                        CHAR(1) NOT NULL,
CCC004*      ST_BEREINIGUNG                 CHAR(1) NOT NULL,
CCC004*      KTY_ST_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_ST_TYP                     SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0001                           *
      ******************************************************************
       01  VIVB0001-GESAMT.
        05  NI-VIVB0001-TAB.
           10 NI-VIVB0001-ELE      PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-VIVB0001            REDEFINES   NI-VIVB0001-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-ST-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-ST-PRUEFZIFFER    PIC S9(04)  COMP.
           10 NI-ST-FREI           PIC S9(04)  COMP.
           10 NI-ST-BEREINIGUNG    PIC S9(04)  COMP.
           10 NI-KTY-ST-TYP        PIC S9(04)  COMP.
           10 NI-KNR-ST-TYP        PIC S9(04)  COMP.
        05  DCL-VIVB0001.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 ST-LAST-UPDATE       PIC X(26).
           10 ST-PRUEFZIFFER       PIC X(1).
           10 ST-FREI              PIC X(1).
           10 ST-BEREINIGUNG       PIC X(1).
           10 KTY-ST-TYP           PIC S9(4) USAGE COMP.
           10 KNR-ST-TYP           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
