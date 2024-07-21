      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0036.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0036.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:59:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( TBVB0036)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0036))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TBVB0036 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      GEBG_MONAT                     DATE NOT NULL,
CCC004*      KTY_GEBG_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_GEBG_TYP                   SMALLINT NOT NULL,
CCC004*      GEBG_AE_PROZ                   DECIMAL(5, 2) NOT NULL,
CCC004*      GEBG_EQF_EIG                   DECIMAL(5, 2) NOT NULL,
CCC004*      GEBG_EQF_GRU                   DECIMAL(5, 2) NOT NULL,
CCC004*      GEBG_EQF_GES                   DECIMAL(5, 2) NOT NULL,
CCC004*      GEBG_ZUG_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_ZUG_EH_GES                DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_ZUG_EH_GRU                DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_ABG_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_ABG_EH_GES                DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_ABG_EH_GRU                DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_N_EH_EIG                  DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_N_EH_GES                  DECIMAL(10, 1) NOT NULL,
CCC004*      GEBG_N_EH_GRU                  DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBVB0036                          *
      ******************************************************************
       01  TBVB0036-GESAMT.
        05  NI-TBVB0036-TAB.
           10  NI-TBVB0036-ELE     PIC S9(04)  COMP   OCCURS  17 TIMES.
        05  NI-TBVB0036            REDEFINES   NI-TBVB0036-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-GEBG-MONAT        PIC S9(04)  COMP.
           10 NI-KTY-GEBG-TYP      PIC S9(04)  COMP.
           10 NI-KNR-GEBG-TYP      PIC S9(04)  COMP.
           10 NI-GEBG-AE-PROZ      PIC S9(04)  COMP.
           10 NI-GEBG-EQF-EIG      PIC S9(04)  COMP.
           10 NI-GEBG-EQF-GRU      PIC S9(04)  COMP.
           10 NI-GEBG-EQF-GES      PIC S9(04)  COMP.
           10 NI-GEBG-ZUG-EH-EIG   PIC S9(04)  COMP.
           10 NI-GEBG-ZUG-EH-GES   PIC S9(04)  COMP.
           10 NI-GEBG-ZUG-EH-GRU   PIC S9(04)  COMP.
           10 NI-GEBG-ABG-EH-EIG   PIC S9(04)  COMP.
           10 NI-GEBG-ABG-EH-GES   PIC S9(04)  COMP.
           10 NI-GEBG-ABG-EH-GRU   PIC S9(04)  COMP.
           10 NI-GEBG-N-EH-EIG     PIC S9(04)  COMP.
           10 NI-GEBG-N-EH-GES     PIC S9(04)  COMP.
           10 NI-GEBG-N-EH-GRU     PIC S9(04)  COMP.
        05  DCL-TBVB0036.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 GEBG-MONAT           PIC X(10).
           10 KTY-GEBG-TYP         PIC S9(4) USAGE COMP.
           10 KNR-GEBG-TYP         PIC S9(4) USAGE COMP.
           10 GEBG-AE-PROZ         PIC S999V99 USAGE COMP-3.
           10 GEBG-EQF-EIG         PIC S999V99 USAGE COMP-3.
           10 GEBG-EQF-GRU         PIC S999V99 USAGE COMP-3.
           10 GEBG-EQF-GES         PIC S999V99 USAGE COMP-3.
           10 GEBG-ZUG-EH-EIG      PIC S999999999V9 USAGE COMP-3.
           10 GEBG-ZUG-EH-GES      PIC S999999999V9 USAGE COMP-3.
           10 GEBG-ZUG-EH-GRU      PIC S999999999V9 USAGE COMP-3.
           10 GEBG-ABG-EH-EIG      PIC S999999999V9 USAGE COMP-3.
           10 GEBG-ABG-EH-GES      PIC S999999999V9 USAGE COMP-3.
           10 GEBG-ABG-EH-GRU      PIC S999999999V9 USAGE COMP-3.
           10 GEBG-N-EH-EIG        PIC S999999999V9 USAGE COMP-3.
           10 GEBG-N-EH-GES        PIC S999999999V9 USAGE COMP-3.
           10 GEBG-N-EH-GRU        PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
