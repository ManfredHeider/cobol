      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0053.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0053.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:02:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( TBVB0053)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0053))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TBVB0053 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      KOST_MONAT                     DATE NOT NULL,
CCC004*      KOST_ZUG_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      KOST_ZUG_EH_GRU                DECIMAL(10, 1) NOT NULL,
CCC004*      KOST_ABG_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      KOST_ABG_EH_GRU                DECIMAL(10, 1) NOT NULL,
CCC004*      KOST_N_EH_EIG                  DECIMAL(10, 1) NOT NULL,
CCC004*      KOST_N_EH_GRU                  DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBVB0053                          *
      ******************************************************************
       01  TBVB0053-GESAMT.
        05  NI-TBVB0053-TAB.
           10  NI-TBVB0053-ELE     PIC S9(04)  COMP   OCCURS   8 TIMES.
        05  NI-TBVB0053            REDEFINES   NI-TBVB0053-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-KOST-MONAT        PIC S9(04)  COMP.
           10 NI-KOST-ZUG-EH-EIG   PIC S9(04)  COMP.
           10 NI-KOST-ZUG-EH-GRU   PIC S9(04)  COMP.
           10 NI-KOST-ABG-EH-EIG   PIC S9(04)  COMP.
           10 NI-KOST-ABG-EH-GRU   PIC S9(04)  COMP.
           10 NI-KOST-N-EH-EIG     PIC S9(04)  COMP.
           10 NI-KOST-N-EH-GRU     PIC S9(04)  COMP.
        05  DCL-TBVB0053.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 KOST-MONAT           PIC X(10).
           10 KOST-ZUG-EH-EIG      PIC S999999999V9 USAGE COMP-3.
           10 KOST-ZUG-EH-GRU      PIC S999999999V9 USAGE COMP-3.
           10 KOST-ABG-EH-EIG      PIC S999999999V9 USAGE COMP-3.
           10 KOST-ABG-EH-GRU      PIC S999999999V9 USAGE COMP-3.
           10 KOST-N-EH-EIG        PIC S999999999V9 USAGE COMP-3.
           10 KOST-N-EH-GRU        PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  8      *
      ******************************************************************
