      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0033.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:24:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0033                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GRUPPE AS                                        * 00060002
      *    DATUM:    06.10.1994                                       * 00070001
      *    STAND:    06.10.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  2    15.11.94 09:18:47
      ***************************************************************** 00090000
      *                                                                 00100000
      *    AFVB0142  NI-TABELLE OCCURS KORRIGIERT  11.94  M.KUHN        00100000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0033                           *
      ******************************************************************
        05  WB-NI-VIVB0033-TAB.
           10 WB-NI-VIVB0033-ELE     PIC S9(04) COMP  OCCURS 11 TIMES.
        05  WB-NI-VIVB0033         REDEFINES  WB-NI-VIVB0033-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-EV-MONAT         PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-VERGUET-TYP  PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-VERGUET-TYP  PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-EV-EQF           PIC S9(4) USAGE COMP.
           10 WB-NI-EV-STORMIND-PROZ PIC S9(4) USAGE COMP.
           10 WB-NI-EV-AE-MONATS-DS  PIC S9(4) USAGE COMP.
           10 WB-NI-EV-AUSZAHLUNG    PIC S9(4) USAGE COMP.
           10 WB-NI-EV-BRUTTO-BETRAG PIC S9(4) USAGE COMP.
        05  WB-VIVB0033.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-EV-MONAT            PIC X(10).
           10 WB-KNR-VERGUET-TYP     PIC S9(4) USAGE COMP.
           10 WB-KNR-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KTY-VERGUET-TYP     PIC S9(4) USAGE COMP.
           10 WB-KTY-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-EV-EQF              PIC S999V99 USAGE COMP-3.
           10 WB-EV-STORMIND-PROZ    PIC S999V99 USAGE COMP-3.
           10 WB-EV-AE-MONATS-DS     PIC S999999999V9 USAGE COMP-3.
           10 WB-EV-AUSZAHLUNG       PIC S999999999V99 USAGE COMP-3.
           10 WB-EV-BRUTTO-BETRAG    PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
