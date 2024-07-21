      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:10:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0001                                      * 00022001
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082001
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0001                           *
      ******************************************************************
        05  WB-NI-VIAT0001-TAB.
           10 WB-NI-VIAT0001-ELE   PIC S9(04) COMP  OCCURS 6 TIMES.
        05  WB-NI-VIAT0001         REDEFINES  WB-NI-VIAT0001-TAB.
           10 WB-NI-DI-TRANS-ID    PIC S9(4)  COMP.
           10 WB-NI-DI-LAST-UPDATE PIC S9(4)  COMP.
           10 WB-NI-DI-TRANS-UP    PIC S9(4)  COMP.
           10 WB-NI-DI-PROG-NAME   PIC S9(4)  COMP.
           10 WB-NI-DI-TYP         PIC S9(4)  COMP.
           10 WB-NI-DI-BILD-ANZ    PIC S9(4)  COMP.
        05 WB-VIAT0001.
           10 WB-DI-TRANS-ID       PIC X(4)              VALUE SPACE.
           10 WB-DI-LAST-UPDATE    PIC X(26)             VALUE SPACE.
           10 WB-DI-TRANS-UP       PIC X(4)              VALUE SPACE.
           10 WB-DI-PROG-NAME      PIC X(8)              VALUE SPACE.
           10 WB-DI-TYP            PIC X(1)              VALUE SPACE.
           10 WB-DI-BILD-ANZ       PIC S9(4)  COMP       VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
