      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0009.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0009.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:12:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0009                                      * 00022003
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0009                           *
      ******************************************************************
        05  WB-NI-VIAT0009-TAB.
           10 WB-NI-VIAT0009-ELE    PIC S9(04) COMP  OCCURS 4 TIMES.
        05  WB-NI-VIAT0009          REDEFINES  WB-NI-VIAT0009-TAB.
           10 WB-NI-HBL-HBILD-ID    PIC S9(4) USAGE COMP.
           10 WB-NI-HBL-LAST-UPDATE PIC S9(4) USAGE COMP.
           10 WB-NI-HBL-HBILDTYP    PIC S9(4) USAGE COMP.
           10 WB-NI-HBL-HILFEGEBIET PIC S9(4) USAGE COMP.
        05  WB-VIAT0009.
           10 WB-HBL-HBILD-ID       PIC X(6)              VALUE SPACE.
           10 WB-HBL-LAST-UPDATE    PIC X(26)             VALUE SPACE.
           10 WB-HBL-HBILDTYP       PIC X(1)              VALUE SPACE.
           10 WB-HBL-HILFEGEBIET    PIC X(66)             VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
