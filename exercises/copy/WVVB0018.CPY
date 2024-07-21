      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:20:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0018                                      * 00022005
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0018                           *
      ******************************************************************
        05  WB-NI-VIVB0018-TAB.
           10  WB-NI-VIVB0018-ELE  PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  WB-NI-VIVB0018         REDEFINES   WB-NI-VIVB0018-TAB.
           10 WB-NI-ST-NR          PIC S9(04)  COMP.
           10 WB-NI-DIR-NR         PIC S9(04)  COMP.
           10 WB-NI-DZ-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-DZ-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-DZ-DAT-BIS     PIC S9(04)  COMP.
        05  WB-VIVB0018.
           10 WB-ST-NR             PIC S9(9)   COMP         VALUE ZERO.
           10 WB-DIR-NR            PIC S9(4)   COMP         VALUE ZERO.
           10 WB-DZ-DAT-VON        PIC X(26)                VALUE SPACE.
           10 WB-DZ-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-DZ-DAT-BIS        PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
