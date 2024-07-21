      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0058.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0058.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:28:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0058                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GRUPPE AS                                        * 00060002
      *    DATUM:    06.10.1994                                       * 00070001
      *    STAND:    06.10.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    29.10.94 10:26:23
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0058                           *
      ******************************************************************
        05  WB-NI-VIVB0058-TAB.
           10 WB-NI-VIVB0058-ELE     PIC S9(04) COMP  OCCURS 18 TIMES.
        05  WB-NI-VIVB0058         REDEFINES  WB-NI-VIVB0058-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-MONAT       PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-PROD-SPARTE  PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-SPARTE  PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG     PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-ZU-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-ZU-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-AB-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-GS-AB-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-GS-NT-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-GS-NT-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-FG-NT-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-PV-FG-NT-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-ZU-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-ZU-GR PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-AB-EG PIC S9(4) USAGE COMP.
           10 WB-NI-PSEV-EH-FG-AB-GR PIC S9(4) USAGE COMP.
        05  WB-VIVB0058.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-PSEV-MONAT          PIC X(10).
           10 WB-KNR-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 WB-KNR-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 WB-KTY-WAEHRUNG        PIC S9(4) USAGE COMP.
           10 WB-PSEV-EH-GS-ZU-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-GS-ZU-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-GS-AB-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-GS-AB-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-PV-GS-NT-EG    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-PV-GS-NT-GR    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-PV-FG-NT-EG    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-PV-FG-NT-GR    PIC S999999999V99 USAGE COMP-3.
           10 WB-PSEV-EH-FG-ZU-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-FG-ZU-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-FG-AB-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-PSEV-EH-FG-AB-GR    PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
