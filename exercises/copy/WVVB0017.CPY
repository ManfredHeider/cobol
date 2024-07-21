      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:20:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0017                                      * 00022005
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0017                           *
      ******************************************************************
        05  WB-NI-VIVB0017-TAB.
           10  WB-NI-VIVB0017-ELE  PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  WB-NI-VIVB0017         REDEFINES   WB-NI-VIVB0017-TAB.
           10 WB-NI-MA-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-RF-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-RF-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-RF-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-RF-PROZ-STZ    PIC S9(04)  COMP.
           10 WB-NI-RF-ABLAUF-DAT  PIC S9(04)  COMP.
           10 WB-NI-KTY-RF-TYP     PIC S9(04)  COMP.
           10 WB-NI-KNR-RF-TYP     PIC S9(04)  COMP.
        05  WB-VIVB0017.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-RF-DAT-VON        PIC X(26)                VALUE SPACE.
           10 WB-RF-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-RF-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-RF-PROZ-STZ       PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-RF-ABLAUF-DAT     PIC X(10)                VALUE SPACE.
           10 WB-KTY-RF-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-RF-TYP        PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
