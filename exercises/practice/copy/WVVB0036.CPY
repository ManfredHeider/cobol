      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0036.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0036.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:24:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0036                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  WB-VIVB0036                       *
      ******************************************************************
        05  WB-NI-VIVB0036-TAB.
           10  WB-NI-VIVB0036-ELE  PIC S9(04)  COMP   OCCURS  17 TIMES.
        05  WB-NI-VIVB0036          REDEFINES   WB-NI-VIVB0036-TAB.
           10 WB-NI-ST-NR           PIC S9(04)  COMP.
           10 WB-NI-GEBG-MONAT      PIC S9(04)  COMP.
           10 WB-NI-KTY-GEBG-TYP    PIC S9(04)  COMP.
           10 WB-NI-KNR-GEBG-TYP    PIC S9(04)  COMP.
           10 WB-NI-GEBG-AE-PROZ    PIC S9(04)  COMP.
           10 WB-NI-GEBG-EQF-EIG    PIC S9(04)  COMP.
           10 WB-NI-GEBG-EQF-GRU    PIC S9(04)  COMP.
           10 WB-NI-GEBG-EQF-GES    PIC S9(04)  COMP.
           10 WB-NI-GEBG-ZUG-EH-EIG PIC S9(04)  COMP.
           10 WB-NI-GEBG-ZUG-EH-GES PIC S9(04)  COMP.
           10 WB-NI-GEBG-ZUG-EH-GRU PIC S9(04)  COMP.
           10 WB-NI-GEBG-ABG-EH-EIG PIC S9(04)  COMP.
           10 WB-NI-GEBG-ABG-EH-GES PIC S9(04)  COMP.
           10 WB-NI-GEBG-ABG-EH-GRU PIC S9(04)  COMP.
           10 WB-NI-GEBG-N-EH-EIG   PIC S9(04)  COMP.
           10 WB-NI-GEBG-N-EH-GES   PIC S9(04)  COMP.
           10 WB-NI-GEBG-N-EH-GRU   PIC S9(04)  COMP.
        05  WB-VIVB0036.
           10 WB-ST-NR             PIC S9(9) USAGE COMP     VALUE ZERO.
           10 WB-GEBG-MONAT        PIC X(10)                VALUE SPACE.
           10 WB-KTY-GEBG-TYP      PIC S9(4) USAGE COMP     VALUE ZERO.
           10 WB-KNR-GEBG-TYP      PIC S9(4) USAGE COMP     VALUE ZERO.
           10 WB-GEBG-AE-PROZ      PIC S999V99 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-EQF-EIG      PIC S999V99 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-EQF-GRU      PIC S999V99 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-EQF-GES      PIC S999V99 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-ZUG-EH-EIG   PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-ZUG-EH-GES   PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-ZUG-EH-GRU   PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-ABG-EH-EIG   PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-ABG-EH-GES   PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-ABG-EH-GRU   PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-N-EH-EIG     PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-N-EH-GES     PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
           10 WB-GEBG-N-EH-GRU     PIC S9(9)V9 USAGE COMP-3 VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
