      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVRZ0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVRZ0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:52:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:14:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    DAS IST EIN TESTMEMBER, KANN WIEDER GELÖSCHT WERDEN        * 00010000
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVRZ0001                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    29.06.94 15:13:09
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0030                           *
      ******************************************************************
        05  WB-NI-VIVB0030-TAB.
           10  WB-NI-VIVB0030-ELE  PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  WB-NI-VIVB0030           REDEFINES   WB-NI-VIVB0030-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-ZV-NR            PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-VON      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-BIS      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-KUE-DAT      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-BIS-R    PIC S9(04)  COMP.
        05  WB-VIVB0030.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-ZV-NR             PIC X(1)                 VALUE SPACE.
           10 WB-ZVZ-DAT-VON       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-ZVZ-DAT-BIS       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-KUE-DAT       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-DAT-BIS-R     PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
