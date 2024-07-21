      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA037.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA037.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:16:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVBA037                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GRUPPE AS                                        * 00060002
      *    DATUM:    06.10.1994                                       * 00070001
      *    STAND:    06.10.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    29.10.94 10:24:42
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA037                           *
      ******************************************************************
        05  WB-NI-VIVBA037-TAB.
           10 WB-NI-VIVBA037-ELE   PIC S9(04) COMP  OCCURS 6 TIMES.
        05  WB-NI-VIVBA037         REDEFINES  WB-NI-VIVBA037-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-GESE-MONAT       PIC S9(4) USAGE COMP.
           10 WB-NI-GESE-EH-GS-ZU-EG PIC S9(4) USAGE COMP.
           10 WB-NI-GESE-EH-GS-ZU-GR PIC S9(4) USAGE COMP.
           10 WB-NI-GESE-EH-GS-AB-EG PIC S9(4) USAGE COMP.
           10 WB-NI-GESE-EH-GS-AB-GR PIC S9(4) USAGE COMP.
        05  WB-VIVBA037.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-GESE-MONAT          PIC X(10).
           10 WB-GESE-EH-GS-ZU-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-GESE-EH-GS-ZU-GR    PIC S999999999V9 USAGE COMP-3.
           10 WB-GESE-EH-GS-AB-EG    PIC S999999999V9 USAGE COMP-3.
           10 WB-GESE-EH-GS-AB-GR    PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
