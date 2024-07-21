      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0056.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0056.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:28:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0056                                      * 00022006
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: M. KUHN                                          * 00060002
      *    DATUM:    12.11.1993                                       * 00070001
      *    STAND:    12.11.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0056                           *
      ******************************************************************
        05  WB-NI-VIVB0056-TAB.
           10  WB-NI-VIVB0056-ELE  PIC S9(04)  COMP   OCCURS   3 TIMES.
        05  WB-NI-VIVB0056         REDEFINES   WB-NI-VIVB0056-TAB.
           10 WB-NI-MS-NR             PIC S9(04)  COMP.
           10 WB-NI-MA-ST-NR          PIC S9(04)  COMP.
           10 WB-NI-UMSF-UMSATZ-KZ    PIC S9(04)  COMP.
        05  WB-VIVB0056.
           10 WB-MS-NR             PIC S9(4) USAGE COMP     VALUE ZERO.
           10 WB-MA-ST-NR          PIC S9(9) USAGE COMP     VALUE ZERO.
           10 WB-UMSF-UMSATZ-KZ    PIC X(1)                 VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
