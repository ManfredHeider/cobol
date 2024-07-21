      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0038.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0038.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:59:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( TBVB0038)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0038))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TBVB0038 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      PE_MONAT                       DATE NOT NULL,
CCC004*      KTY_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      KNR_PROD_SPARTE                SMALLINT NOT NULL,
CCC004*      PE_EQF_EIG                     DECIMAL(5, 2) NOT NULL,
CCC004*      PE_EQF_GES                     DECIMAL(5, 2) NOT NULL,
CCC004*      PE_EQF_GRU                     DECIMAL(5, 2) NOT NULL,
CCC004*      PE_ZUG_EH_EIG                  DECIMAL(10, 1) NOT NULL,
CCC004*      PE_ZUG_EH_GRU                  DECIMAL(10, 1) NOT NULL,
CCC004*      PE_ABG_EH_EIG                  DECIMAL(10, 1) NOT NULL,
CCC004*      PE_ABG_EH_GRU                  DECIMAL(10, 1) NOT NULL,
CCC004*      PE_N_EH_EIG                    DECIMAL(10, 1) NOT NULL,
CCC004*      PE_N_EH_GRU                    DECIMAL(10, 1) NOT NULL,
CCC004*      PE_FG_ZUG_EH_EIG               DECIMAL(10, 1) NOT NULL,
CCC004*      PE_FG_ZUG_EH_GRU               DECIMAL(10, 1) NOT NULL,
CCC004*      PE_FG_ABG_EH_EIG               DECIMAL(10, 1) NOT NULL,
CCC004*      PE_FG_ABG_EH_GRU               DECIMAL(10, 1) NOT NULL,
CCC004*      PE_FG_N_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      PE_FG_N_EH_GRU                 DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBVB0038                          *
      ******************************************************************
       01  TBVB0038-GESAMT.
        05  NI-TBVB0038-TAB.
           10  NI-TBVB0038-ELE     PIC S9(04)  COMP   OCCURS  19 TIMES.
        05  NI-TBVB0038            REDEFINES   NI-TBVB0038-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-PE-MONAT          PIC S9(04)  COMP.
           10 NI-KTY-PROD-SPARTE   PIC S9(04)  COMP.
           10 NI-KNR-PROD-SPARTE   PIC S9(04)  COMP.
           10 NI-PE-EQF-EIG        PIC S9(04)  COMP.
           10 NI-PE-EQF-GES        PIC S9(04)  COMP.
           10 NI-PE-EQF-GRU        PIC S9(04)  COMP.
           10 NI-PE-ZUG-EH-EIG     PIC S9(04)  COMP.
           10 NI-PE-ZUG-EH-GRU     PIC S9(04)  COMP.
           10 NI-PE-ABG-EH-EIG     PIC S9(04)  COMP.
           10 NI-PE-ABG-EH-GRU     PIC S9(04)  COMP.
           10 NI-PE-N-EH-EIG       PIC S9(04)  COMP.
           10 NI-PE-N-EH-GRU       PIC S9(04)  COMP.
           10 NI-PE-FG-ZUG-EH-EIG  PIC S9(04)  COMP.
           10 NI-PE-FG-ZUG-EH-GRU  PIC S9(04)  COMP.
           10 NI-PE-FG-ABG-EH-EIG  PIC S9(04)  COMP.
           10 NI-PE-FG-ABG-EH-GRU  PIC S9(04)  COMP.
           10 NI-PE-FG-N-EH-EIG    PIC S9(04)  COMP.
           10 NI-PE-FG-N-EH-GRU    PIC S9(04)  COMP.
        05  DCL-TBVB0038.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 PE-MONAT             PIC X(10).
           10 KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 PE-EQF-EIG           PIC S999V99 USAGE COMP-3.
           10 PE-EQF-GES           PIC S999V99 USAGE COMP-3.
           10 PE-EQF-GRU           PIC S999V99 USAGE COMP-3.
           10 PE-ZUG-EH-EIG        PIC S999999999V9 USAGE COMP-3.
           10 PE-ZUG-EH-GRU        PIC S999999999V9 USAGE COMP-3.
           10 PE-ABG-EH-EIG        PIC S999999999V9 USAGE COMP-3.
           10 PE-ABG-EH-GRU        PIC S999999999V9 USAGE COMP-3.
           10 PE-N-EH-EIG          PIC S999999999V9 USAGE COMP-3.
           10 PE-N-EH-GRU          PIC S999999999V9 USAGE COMP-3.
           10 PE-FG-ZUG-EH-EIG     PIC S999999999V9 USAGE COMP-3.
           10 PE-FG-ZUG-EH-GRU     PIC S999999999V9 USAGE COMP-3.
           10 PE-FG-ABG-EH-EIG     PIC S999999999V9 USAGE COMP-3.
           10 PE-FG-ABG-EH-GRU     PIC S999999999V9 USAGE COMP-3.
           10 PE-FG-N-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 PE-FG-N-EH-GRU       PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
