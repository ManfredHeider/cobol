      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0054.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0054.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:28:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0054                                      * 00022008
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0054                           *
      ******************************************************************
        05  WB-NI-VIVB0054-TAB.
           10 WB-NI-VIVB0054-ELE     PIC S9(04) COMP  OCCURS 12 TIMES.
        05  WB-NI-VIVB0054           REDEFINES  WB-NI-VIVB0054-TAB.
           10 WB-NI-WV-TABLE-ID      PIC S9(4) USAGE COMP.
           10 WB-NI-WV-MA-NR         PIC S9(4) USAGE COMP.
           10 WB-NI-WV-TIMESTAMP     PIC S9(4) USAGE COMP.
           10 WB-NI-WV-DAT-AEND      PIC S9(4) USAGE COMP.
           10 WB-NI-WV-DAT-VON       PIC S9(4) USAGE COMP.
           10 WB-NI-WV-PROGRAMM-ID   PIC S9(4) USAGE COMP.
           10 WB-NI-WV-USER-ID       PIC S9(4) USAGE COMP.
           10 WB-NI-WV-TERMINAL-ID   PIC S9(4) USAGE COMP.
           10 WB-NI-WV-FELDNAME      PIC S9(4) USAGE COMP.
           10 WB-NI-WV-FELDTYP       PIC S9(4) USAGE COMP.
           10 WB-NI-WV-AEND-LANG     PIC S9(4) USAGE COMP.
           10 WB-NI-WV-AEND-WERT     PIC S9(4) USAGE COMP.
        05 WB-VIVB0054.
           10 WB-WV-TABLE-ID       PIC X(8)                VALUE SPACE.
           10 WB-WV-MA-NR          PIC S9(9) USAGE COMP    VALUE ZERO.
           10 WB-WV-TIMESTAMP      PIC X(26)               VALUE SPACE.
           10 WB-WV-DAT-AEND       PIC X(10)               VALUE SPACE.
           10 WB-WV-DAT-VON        PIC X(10)               VALUE SPACE.
           10 WB-WV-PROGRAMM-ID    PIC X(8)                VALUE SPACE.
           10 WB-WV-USER-ID        PIC X(8)                VALUE SPACE.
           10 WB-WV-TERMINAL-ID    PIC X(8)                VALUE SPACE.
           10 WB-WV-FELDNAME       PIC X(16)               VALUE SPACE.
           10 WB-WV-FELDTYP        PIC X(1)                VALUE SPACE.
           10 WB-WV-AEND-LANG      PIC S9(9) USAGE COMP    VALUE ZERO.
           10 WB-WV-AEND-WERT      PIC X(70)               VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
