      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:10:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0002                                      * 00022003
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0002                           *
      ******************************************************************
        05  WB-NI-VIAT0002-TAB.
           10 WB-NI-VIAT0002-ELE   PIC S9(04) COMP  OCCURS 5 TIMES.
        05  WB-NI-VIAT0002         REDEFINES  WB-NI-VIAT0002-TAB.
           10 WB-NI-ERR-PROG-NAME   PIC S9(4) COMP.
           10 WB-NI-ERR-TYP         PIC S9(4) COMP.
           10 WB-NI-ERR-NR          PIC S9(4) COMP.
           10 WB-NI-ERR-LAST-UPDATE PIC S9(4) COMP.
           10 WB-NI-ERR-TEXT        PIC S9(4) COMP.
        05 WB-VIAT0002.
           10 WB-ERR-PROG-NAME     PIC X(8)               VALUE SPACE.
           10 WB-ERR-TYP           PIC X(1)               VALUE SPACE.
           10 WB-ERR-NR            PIC S9(4) COMP         VALUE ZERO.
           10 WB-ERR-LAST-UPDATE   PIC X(26)              VALUE SPACE.
           10 WB-ERR-TEXT          PIC X(70)              VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
