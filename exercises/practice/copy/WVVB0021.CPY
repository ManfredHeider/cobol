      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:21:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0021                                      * 00022006
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0021                           *
      ******************************************************************
        05  WB-NI-VIVB0021-TAB.
           10  WB-NI-VIVB0021-ELE  PIC S9(04)  COMP   OCCURS  12 TIMES.
        05  WB-NI-VIVB0021         REDEFINES   WB-NI-VIVB0021-TAB.
           10 WB-NI-ST-NR          PIC S9(04)  COMP.
           10 WB-NI-KTY-BV-TYP     PIC S9(04)  COMP.
           10 WB-NI-KNR-BV-TYP     PIC S9(04)  COMP.
           10 WB-NI-BV-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-BV-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-BV-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KTY-SPERRE     PIC S9(04)  COMP.
           10 WB-NI-KNR-SPERRE     PIC S9(04)  COMP.
           10 WB-NI-BV-KTO-NR      PIC S9(04)  COMP.
           10 WB-NI-BV-BLZ         PIC S9(04)  COMP.
           10 WB-NI-BV-BANKNAME    PIC S9(04)  COMP.
           10 WB-NI-BV-KTO-INHABER PIC S9(04)  COMP.
        05  WB-VIVB0021.
           10 WB-ST-NR             PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KTY-BV-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BV-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-BV-DAT-VON        PIC X(26)                VALUE SPACE.
           10 WB-BV-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-BV-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-KTY-SPERRE        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-SPERRE        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-BV-KTO-NR         PIC X(20)                VALUE SPACE.
           10 WB-BV-BLZ            PIC X(11)                VALUE SPACE.
           10 WB-BV-BANKNAME       PIC X(40)                VALUE SPACE.
           10 WB-BV-KTO-INHABER    PIC X(40)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
