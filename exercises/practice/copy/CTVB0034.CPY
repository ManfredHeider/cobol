      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0034.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0034.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:58:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( TBVB0034)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0034))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TBVB0034 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      PV_MONAT                       DATE NOT NULL,
CCC004*      KTY_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      KNR_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      PV_VERD_EIG                    DECIMAL(11, 2) NOT NULL,
CCC004*      PV_VERD_GRU                    DECIMAL(11, 2) NOT NULL,
CCC004*      PV_FG_VERD_EIG                 DECIMAL(11, 2) NOT NULL,
CCC004*      PV_FG_VERD_GRU                 DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBVB0034                          *
      ******************************************************************
       01  TBVB0034-GESAMT.
        05  NI-TBVB0034-TAB.
           10  NI-TBVB0034-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-TBVB0034            REDEFINES   NI-TBVB0034-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-PV-MONAT          PIC S9(04)  COMP.
           10 NI-KTY-PROD-SPARTE   PIC S9(04)  COMP.
           10 NI-KNR-PROD-SPARTE   PIC S9(04)  COMP.
           10 NI-PV-VERD-EIG       PIC S9(04)  COMP.
           10 NI-PV-VERD-GRU       PIC S9(04)  COMP.
           10 NI-PV-FG-VERD-EIG    PIC S9(04)  COMP.
           10 NI-PV-FG-VERD-GRU    PIC S9(04)  COMP.
        05  DCL-TBVB0034.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 PV-MONAT             PIC X(10).
           10 KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 PV-VERD-EIG          PIC S999999999V99 USAGE COMP-3.
           10 PV-VERD-GRU          PIC S999999999V99 USAGE COMP-3.
           10 PV-FG-VERD-EIG       PIC S999999999V99 USAGE COMP-3.
           10 PV-FG-VERD-GRU       PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
