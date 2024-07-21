      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0037.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0037.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:19:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( VIVB0037)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0037))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VIVB0037 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      GESE_MONAT                     DATE NOT NULL,
CCC004*      GESE_EQF_EIG                   DECIMAL(5, 2) NOT NULL,
CCC004*      GESE_EQF_GRU                   DECIMAL(5, 2) NOT NULL,
CCC004*      GESE_EQF_GES                   DECIMAL(5, 2) NOT NULL,
CCC004*      GESE_EH_B_78_GES               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_EH_A_78_EIG               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_EH_A_78_GRU               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_ZUG_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_ZUG_EH_GRU                DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_ABG_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_ABG_EH_GRU                DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_N_EH_EIG                  DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_N_EH_GRU                  DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_FG_N_EH_EIG               DECIMAL(10, 1) NOT NULL,
CCC004*      GESE_FG_N_EH_GRU               DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VIVB0037                          *
      ******************************************************************
       01  VIVB0037-GESAMT.
        05  NI-VIVB0037-TAB.
           10  NI-VIVB0037-ELE     PIC S9(04)  COMP   OCCURS  16 TIMES.
        05  NI-VIVB0037            REDEFINES   NI-VIVB0037-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-GESE-MONAT        PIC S9(04)  COMP.
           10 NI-GESE-EQF-EIG      PIC S9(04)  COMP.
           10 NI-GESE-EQF-GRU      PIC S9(04)  COMP.
           10 NI-GESE-EQF-GES      PIC S9(04)  COMP.
           10 NI-GESE-EH-B-78-GES  PIC S9(04)  COMP.
           10 NI-GESE-EH-A-78-EIG  PIC S9(04)  COMP.
           10 NI-GESE-EH-A-78-GRU  PIC S9(04)  COMP.
           10 NI-GESE-ZUG-EH-EIG   PIC S9(04)  COMP.
           10 NI-GESE-ZUG-EH-GRU   PIC S9(04)  COMP.
           10 NI-GESE-ABG-EH-EIG   PIC S9(04)  COMP.
           10 NI-GESE-ABG-EH-GRU   PIC S9(04)  COMP.
           10 NI-GESE-N-EH-EIG     PIC S9(04)  COMP.
           10 NI-GESE-N-EH-GRU     PIC S9(04)  COMP.
           10 NI-GESE-FG-N-EH-EIG  PIC S9(04)  COMP.
           10 NI-GESE-FG-N-EH-GRU  PIC S9(04)  COMP.
        05  DCL-VIVB0037.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 GESE-MONAT           PIC X(10).
           10 GESE-EQF-EIG         PIC S999V99 USAGE COMP-3.
           10 GESE-EQF-GRU         PIC S999V99 USAGE COMP-3.
           10 GESE-EQF-GES         PIC S999V99 USAGE COMP-3.
           10 GESE-EH-B-78-GES     PIC S999999999V9 USAGE COMP-3.
           10 GESE-EH-A-78-EIG     PIC S999999999V9 USAGE COMP-3.
           10 GESE-EH-A-78-GRU     PIC S999999999V9 USAGE COMP-3.
           10 GESE-ZUG-EH-EIG      PIC S999999999V9 USAGE COMP-3.
           10 GESE-ZUG-EH-GRU      PIC S999999999V9 USAGE COMP-3.
           10 GESE-ABG-EH-EIG      PIC S999999999V9 USAGE COMP-3.
           10 GESE-ABG-EH-GRU      PIC S999999999V9 USAGE COMP-3.
           10 GESE-N-EH-EIG        PIC S999999999V9 USAGE COMP-3.
           10 GESE-N-EH-GRU        PIC S999999999V9 USAGE COMP-3.
           10 GESE-FG-N-EH-EIG     PIC S999999999V9 USAGE COMP-3.
           10 GESE-FG-N-EH-GRU     PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
