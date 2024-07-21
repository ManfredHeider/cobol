      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0037.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0037.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:24:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0037                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  WB-VIVB0037                       *
      ******************************************************************
        05  WB-NI-VIVB0037-TAB.
           10  WB-NI-VIVB0037-ELE  PIC S9(04)  COMP   OCCURS  16 TIMES.
        05  WB-NI-VIVB0037           REDEFINES   WB-NI-VIVB0037-TAB.
           10 WB-NI-ST-NR            PIC S9(04)  COMP.
           10 WB-NI-GESE-MONAT       PIC S9(04)  COMP.
           10 WB-NI-GESE-EQF-EIG     PIC S9(04)  COMP.
           10 WB-NI-GESE-EQF-GRU     PIC S9(04)  COMP.
           10 WB-NI-GESE-EQF-GES     PIC S9(04)  COMP.
           10 WB-NI-GESE-EH-B-78-GES PIC S9(04)  COMP.
           10 WB-NI-GESE-EH-A-78-EIG PIC S9(04)  COMP.
           10 WB-NI-GESE-EH-A-78-GRU PIC S9(04)  COMP.
           10 WB-NI-GESE-ZUG-EH-EIG  PIC S9(04)  COMP.
           10 WB-NI-GESE-ZUG-EH-GRU  PIC S9(04)  COMP.
           10 WB-NI-GESE-ABG-EH-EIG  PIC S9(04)  COMP.
           10 WB-NI-GESE-ABG-EH-GRU  PIC S9(04)  COMP.
           10 WB-NI-GESE-N-EH-EIG    PIC S9(04)  COMP.
           10 WB-NI-GESE-N-EH-GRU    PIC S9(04)  COMP.
           10 WB-NI-GESE-FG-N-EH-EIG PIC S9(04)  COMP.
           10 WB-NI-GESE-FG-N-EH-GRU PIC S9(04)  COMP.
        05  WB-VIVB0037.
           10 WB-ST-NR             PIC S9(9)   COMP         VALUE ZERO.
           10 WB-GESE-MONAT        PIC X(10)                VALUE SPACE.
           10 WB-GESE-EQF-EIG      PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-GESE-EQF-GRU      PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-GESE-EQF-GES      PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-GESE-EH-B-78-GES  PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-EH-A-78-EIG  PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-EH-A-78-GRU  PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-ZUG-EH-EIG   PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-ZUG-EH-GRU   PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-ABG-EH-EIG   PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-ABG-EH-GRU   PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-N-EH-EIG     PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-N-EH-GRU     PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-FG-N-EH-EIG  PIC S9(9)V9   COMP-3     VALUE ZERO.
           10 WB-GESE-FG-N-EH-GRU  PIC S9(9)V9   COMP-3     VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
