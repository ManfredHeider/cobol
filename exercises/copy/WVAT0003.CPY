      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:11:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0003                                      * 00022003
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0003                           *
      ******************************************************************
        05  WB-NI-VIAT0003-TAB.
           10 WB-NI-VIAT0003-ELE   PIC S9(04) COMP  OCCURS 5 TIMES.
        05  WB-NI-VIAT0003         REDEFINES  WB-NI-VIAT0003-TAB.
           10 WB-NI-FKT-PROG-NAME    PIC S9(4) USAGE COMP.
           10 WB-NI-FKT-BILD-ZUSTAND PIC S9(4) USAGE COMP.
           10 WB-NI-FKT-LFD-NR       PIC S9(4) USAGE COMP.
           10 WB-NI-FKT-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 WB-NI-FKT-TEXT         PIC S9(4) USAGE COMP.
        05 WB-VIAT0003.
           10 WB-FKT-PROG-NAME     PIC X(8)                VALUE SPACE.
           10 WB-FKT-BILD-ZUSTAND  PIC X(2)                VALUE SPACE.
           10 WB-FKT-LFD-NR        PIC S9(4) USAGE COMP    VALUE ZERO.
           10 WB-FKT-LAST-UPDATE   PIC X(26)               VALUE SPACE.
           10 WB-FKT-TEXT          PIC X(79)               VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
