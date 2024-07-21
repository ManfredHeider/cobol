      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0052.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0052.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:02:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( TBVB0052)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0052))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TBVB0052 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      JEQF_MONAT                     DATE NOT NULL,
CCC004*      JEQF_EIG                       DECIMAL(5, 2) NOT NULL,
CCC004*      JEQF_GRU                       DECIMAL(5, 2) NOT NULL,
CCC004*      JEQF_GES                       DECIMAL(5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBVB0052                          *
      ******************************************************************
       01  TBVB0052-GESAMT.
        05  NI-TBVB0052-TAB.
           10 NI-TBVB0052-ELE       PIC S9(04)  COMP  OCCURS  5  TIMES.
        05  NI-TBVB0052             REDEFINES   NI-TBVB0052-TAB.
           10 NI-ST-NR              PIC S9(04)  COMP.
           10 NI-JEQF-MONAT         PIC S9(04)  COMP.
           10 NI-JEQF-EIG           PIC S9(04)  COMP.
           10 NI-JEQF-GRU           PIC S9(04)  COMP.
           10 NI-JEQF-GES           PIC S9(04)  COMP.
        05  DCL-TBVB0052.
           10 ST-NR                 PIC S9(9) USAGE COMP.
           10 JEQF-MONAT            PIC X(10).
           10 JEQF-EIG              PIC S999V99 USAGE COMP-3.
           10 JEQF-GRU              PIC S999V99 USAGE COMP-3.
           10 JEQF-GES              PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
