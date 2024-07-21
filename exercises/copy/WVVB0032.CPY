      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0032.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0032.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:23:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0032                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GRUPPE AS                                        * 00060002
      *    DATUM:    06.10.1994                                       * 00070001
      *    STAND:    06.10.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  2    15.11.94 09:18:27
      ***************************************************************** 00090000
      *                                                                 00100000
      *    AFVB0142  NI-TABELLE OCCURS KORRIGIERT  11.94  M.KUHN        00100000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0032                           *
      ******************************************************************
        05  WB-NI-VIVB0032-TAB.
           10 WB-NI-VIVB0032-ELE     PIC S9(04) COMP  OCCURS  9 TIMES.
        05  WB-NI-VIVB0032         REDEFINES  WB-NI-VIVB0032-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-MONAT        PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-UEBWEIS      PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-STAND-DISK   PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-STAND-VORSCH PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-STAND-RUECK  PIC S9(4) USAGE COMP.
           10 WB-NI-KTO-RCK-STZ      PIC S9(4) USAGE COMP.
        05  WB-VIVB0032.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-KTO-MONAT           PIC X(10).
           10 WB-KNR-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KTY-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KTO-UEBWEIS         PIC S999999999V99 USAGE COMP-3.
           10 WB-KTO-STAND-DISK      PIC S999999999V99 USAGE COMP-3.
           10 WB-KTO-STAND-VORSCH    PIC S999999999V99 USAGE COMP-3.
           10 WB-KTO-STAND-RUECK     PIC S999999999V99 USAGE COMP-3.
           10 WB-KTO-RCK-STZ         PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  9      *
      ******************************************************************
