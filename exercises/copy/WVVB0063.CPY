      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0063.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0063.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:29:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0063                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: P. SCHMIDT                                       * 00060002
      *    DATUM:    16.04.1995                                       * 00070001
      *    STAND:    16.04.1995                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    06.05.95 16:07:38
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0063                           *
      ******************************************************************
        05  WB-NI-VIVB0063-TAB.
           10 WB-NI-VIVB0063-ELE     PIC S9(04) COMP  OCCURS  7 TIMES.
        05  WB-NI-VIVB0063         REDEFINES  WB-NI-VIVB0063-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-BER-HAFTUNG      PIC S9(4) USAGE COMP.
           10 WB-NI-BER-L-PROV-BEW   PIC S9(4) USAGE COMP.
           10 WB-NI-BER-STAND-DISK   PIC S9(4) USAGE COMP.
           10 WB-NI-BER-STAND-RUECK  PIC S9(4) USAGE COMP.
           10 WB-NI-BER-STAND-VORSCH PIC S9(4) USAGE COMP.
           10 WB-NI-BER-STAND-ABSCHR PIC S9(4) USAGE COMP.
        05  WB-VIVB0063.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-BER-HAFTUNG         PIC X(1).
           10 WB-BER-L-PROV-BEW      PIC X(1).
           10 WB-BER-STAND-DISK      PIC S999999999V99 USAGE COMP-3.
           10 WB-BER-STAND-RUECK     PIC S999999999V99 USAGE COMP-3.
           10 WB-BER-STAND-VORSCH    PIC S999999999V99 USAGE COMP-3.
           10 WB-BER-STAND-ABSCHR    PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  7      *
      ******************************************************************
