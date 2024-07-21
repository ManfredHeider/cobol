      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0035.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0035.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:18:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( VIVB0035)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0035))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VIVB0035 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      ANT_MONAT                      DATE NOT NULL,
CCC004*      KTY_ANT_TYP                    SMALLINT NOT NULL,
CCC004*      KNR_ANT_TYP                    SMALLINT NOT NULL,
CCC004*      ANT_ZUG_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_ZUG_EH_GES                 DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_ZUG_EH_GRU                 DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_ABG_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_ABG_EH_GES                 DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_ABG_EH_GRU                 DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_N_EH_EIG                   DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_N_EH_GES                   DECIMAL(10, 1) NOT NULL,
CCC004*      ANT_N_EH_GRU                   DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VIVB0035                          *
      ******************************************************************
       01  VIVB0035-GESAMT.
        05  NI-VIVB0035-TAB.
           10  NI-VIVB0035-ELE     PIC S9(04)  COMP   OCCURS  13 TIMES.
        05  NI-VIVB0035            REDEFINES   NI-VIVB0035-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-ANT-MONAT         PIC S9(04)  COMP.
           10 NI-KTY-ANT-TYP       PIC S9(04)  COMP.
           10 NI-KNR-ANT-TYP       PIC S9(04)  COMP.
           10 NI-ANT-ZUG-EH-EIG    PIC S9(04)  COMP.
           10 NI-ANT-ZUG-EH-GES    PIC S9(04)  COMP.
           10 NI-ANT-ZUG-EH-GRU    PIC S9(04)  COMP.
           10 NI-ANT-ABG-EH-EIG    PIC S9(04)  COMP.
           10 NI-ANT-ABG-EH-GES    PIC S9(04)  COMP.
           10 NI-ANT-ABG-EH-GRU    PIC S9(04)  COMP.
           10 NI-ANT-N-EH-EIG      PIC S9(04)  COMP.
           10 NI-ANT-N-EH-GES      PIC S9(04)  COMP.
           10 NI-ANT-N-EH-GRU      PIC S9(04)  COMP.
        05  DCL-VIVB0035.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 ANT-MONAT            PIC X(10).
           10 KTY-ANT-TYP          PIC S9(4) USAGE COMP.
           10 KNR-ANT-TYP          PIC S9(4) USAGE COMP.
           10 ANT-ZUG-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 ANT-ZUG-EH-GES       PIC S999999999V9 USAGE COMP-3.
           10 ANT-ZUG-EH-GRU       PIC S999999999V9 USAGE COMP-3.
           10 ANT-ABG-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 ANT-ABG-EH-GES       PIC S999999999V9 USAGE COMP-3.
           10 ANT-ABG-EH-GRU       PIC S999999999V9 USAGE COMP-3.
           10 ANT-N-EH-EIG         PIC S999999999V9 USAGE COMP-3.
           10 ANT-N-EH-GES         PIC S999999999V9 USAGE COMP-3.
           10 ANT-N-EH-GRU         PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
