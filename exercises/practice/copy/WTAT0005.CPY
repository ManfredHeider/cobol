      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTAT0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTAT0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:09:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WTAT0005                                      * 00022008
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0005                           *
      ******************************************************************
         05 WB-NI-TBAT0005-TAB.
           10 WB-NI-TBAT0005-ELE      PIC S9(4) USAGE COMP  OCCURS  12.
         05 WB-NI-TBAT0005            REDEFINES WB-NI-TBAT0005-TAB.
           10 WB-NI-LOG-TIMESTAMP     PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-TABLE-ID      PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-MA-NR         PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-DAT-AB        PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-DAT-AEND      PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-PROGRAMM-ID   PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-USER-ID       PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-TERMINAL-ID   PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-AEND-KZ       PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-AEND-TYP      PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-TUPEL-ALT-GS  PIC S9(4) USAGE COMP.
           10 WB-NI-LOG-TUPEL-NEU-GS  PIC S9(4) USAGE COMP.
         05  WB-TBAT0005.
           10 WB-LOG-TIMESTAMP        PIC X(26)            VALUE SPACE.
           10 WB-LOG-TABLE-ID         PIC X(8)             VALUE SPACE.
           10 WB-LOG-MA-NR            PIC S9(9) USAGE COMP VALUE ZERO.
           10 WB-LOG-DAT-AB           PIC X(10)            VALUE SPACE.
           10 WB-LOG-DAT-AEND         PIC X(10)            VALUE SPACE.
           10 WB-LOG-PROGRAMM-ID      PIC X(8)             VALUE SPACE.
           10 WB-LOG-USER-ID          PIC X(8)             VALUE SPACE.
           10 WB-LOG-TERMINAL-ID      PIC X(8)             VALUE SPACE.
           10 WB-LOG-AEND-KZ          PIC X(1)             VALUE SPACE.
           10 WB-LOG-AEND-TYP         PIC X(1)             VALUE SPACE.
           10 WB-LOG-TUPEL-ALT-GS.
              49 WB-LOG-TUPEL-ALT-GS-LEN    PIC S9(4) USAGE COMP.
              49 WB-LOG-TUPEL-ALT-GS-TEXT   PIC X(1990).
           10 WB-LOG-TUPEL-NEU-GS.
              49 WB-LOG-TUPEL-NEU-GS-LEN    PIC S9(4) USAGE COMP.
              49 WB-LOG-TUPEL-NEU-GS-TEXT   PIC X(1990).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
