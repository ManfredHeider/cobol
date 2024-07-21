      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVATA007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVATA007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   11 Aug 1997 15:07:32   T13EX23
      *afpa0011 - COPY-NAME eingefuegt
      *
      *   Rev 1.0   11 Aug 1997 09:45:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVATA007                                      * 00022003
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: B. Bohlinger                                     * 00060002
      *    DATUM:    22.04.1997                                       * 00070001
      *    STAND:    22.04.1997                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA007                           *
      ******************************************************************
        05  WB-NI-VIATA007-TAB.
           10  WB-NI-VIATA007-ELE  PIC S9(04)  COMP   OCCURS  4  TIMES.
        05  WB-NI-VIATA007         REDEFINES   WB-NI-VIATA007-TAB.
           10 WB-NI-KTY-NR          PIC S9(04)  COMP.
           10 WB-NI-KTY-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZ         PIC S9(04)  COMP.
           10 WB-NI-KTY-COPY-NAME   PIC S9(04)  COMP.
        05  WB-VIATA007.
           10 WB-KTY-NR            PIC S9(4)  COMP          VALUE ZERO.
           10 WB-KTY-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-KTY-BEZ           PIC X(40)                VALUE SPACE.
           10 WB-KTY-COPY-NAME     PIC X(08)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
