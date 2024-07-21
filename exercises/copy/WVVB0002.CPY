      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:17:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0002                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0002                           *
      ******************************************************************
        05  WB-NI-VIVB0002-TAB.
           10  WB-NI-VIVB0002-ELE  PIC S9(04) COMP   OCCURS  6  TIMES.
        05  WB-NI-VIVB0002         REDEFINES  WB-NI-VIVB0002-TAB.
           10 WB-NI-GK-ST-NR       PIC S9(04) COMP.
           10 WB-NI-GK-LAST-UPDATE PIC S9(04) COMP.
           10 WB-NI-GK-FUNKTION    PIC S9(04) COMP.
           10 WB-NI-GK-DAT-VON     PIC S9(04) COMP.
           10 WB-NI-GK-DAT-BIS     PIC S9(04) COMP.
           10 WB-NI-GK-BEZ         PIC S9(04) COMP.
        05  WB-VIVB0002.
           10 WB-GK-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-GK-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-GK-FUNKTION       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-GK-DAT-VON        PIC X(10)                VALUE SPACE.
           10 WB-GK-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-GK-BEZ            PIC X(40)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
