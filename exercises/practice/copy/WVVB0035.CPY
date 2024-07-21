      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0035.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0035.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:24:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0035                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  WB-VIVB0035                       *
      ******************************************************************
        05  WB-NI-VIVB0035-TAB.
           10  WB-NI-VIVB0035-ELE  PIC S9(04)  COMP   OCCURS  13 TIMES.
        05  WB-NI-VIVB0035         REDEFINES   WB-NI-VIVB0035-TAB.
           10 WB-NI-ST-NR          PIC S9(04)  COMP.
           10 WB-NI-ANT-MONAT      PIC S9(04)  COMP.
           10 WB-NI-KTY-ANT-TYP    PIC S9(04)  COMP.
           10 WB-NI-KNR-ANT-TYP    PIC S9(04)  COMP.
           10 WB-NI-ANT-ZUG-EH-EIG PIC S9(04)  COMP.
           10 WB-NI-ANT-ZUG-EH-GES PIC S9(04)  COMP.
           10 WB-NI-ANT-ZUG-EH-GRU PIC S9(04)  COMP.
           10 WB-NI-ANT-ABG-EH-EIG PIC S9(04)  COMP.
           10 WB-NI-ANT-ABG-EH-GES PIC S9(04)  COMP.
           10 WB-NI-ANT-ABG-EH-GRU PIC S9(04)  COMP.
           10 WB-NI-ANT-N-EH-EIG   PIC S9(04)  COMP.
           10 WB-NI-ANT-N-EH-GES   PIC S9(04)  COMP.
           10 WB-NI-ANT-N-EH-GRU   PIC S9(04)  COMP.
        05  WB-VIVB0035.
           10 WB-ST-NR             PIC S9(9) USAGE COMP     VALUE ZERO.
           10 WB-ANT-MONAT         PIC X(10)                VALUE SPACE.
           10 WB-KTY-ANT-TYP       PIC S9(4) USAGE COMP     VALUE ZERO.
           10 WB-KNR-ANT-TYP       PIC S9(4) USAGE COMP     VALUE ZERO.
           10 WB-ANT-ZUG-EH-EIG    PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-ZUG-EH-GES    PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-ZUG-EH-GRU    PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-ABG-EH-EIG    PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-ABG-EH-GES    PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-ABG-EH-GRU    PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-N-EH-EIG      PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-N-EH-GES      PIC S9(9)V9 COMP-3       VALUE ZERO.
           10 WB-ANT-N-EH-GRU      PIC S9(9)V9 COMP-3       VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
