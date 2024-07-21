      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0040.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0040.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:25:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0040                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0040                           *
      ******************************************************************
        05  WB-NI-VIVB0040-TAB.
           10  WB-NI-VIVB0040-ELE  PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-VIVB0040         REDEFINES   WB-NI-VIVB0040-TAB.
           10 WB-NI-KL-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-KL-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-KL-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-KL-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KL-PROV-STZ-AP PIC S9(04)  COMP.
           10 WB-NI-KL-PROV-STZ-GP PIC S9(04)  COMP.
           10 WB-NI-KL-PROZ-STZ    PIC S9(04)  COMP.
           10 WB-NI-KL-RCK-STZ-DAT PIC S9(04)  COMP.
           10 WB-NI-KL-RCK-STZ     PIC S9(04)  COMP.
        05  WB-VIVB0040.
           10 WB-KL-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KL-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KL-DAT-VON        PIC X(10)                VALUE SPACE.
           10 WB-KL-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-KL-PROV-STZ-AP    PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-PROV-STZ-GP    PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-PROZ-STZ       PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-KL-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
