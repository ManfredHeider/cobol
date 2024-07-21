      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0022.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0022.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:21:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0022                                      * 00022006
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0022                           *
      ******************************************************************
        05  WB-NI-VIVB0022-TAB.
           10  WB-NI-VIVB0022-ELE  PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  WB-NI-VIVB0022           REDEFINES   WB-NI-VIVB0022-TAB.
           10 WB-NI-EDED-BETR-NR     PIC S9(04)  COMP.
           10 WB-NI-EDED-UNTST-NR    PIC S9(04)  COMP.
           10 WB-NI-EDED-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-EDED-LAST-UPDATE PIC S9(04) COMP.
           10 WB-NI-EDED-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KTY-BETR-TYP     PIC S9(04)  COMP.
           10 WB-NI-KNR-BETR-TYP     PIC S9(04)  COMP.
        05  WB-VIVB0022.
           10 WB-EDED-BETR-NR      PIC S9(9)   COMP         VALUE ZERO.
           10 WB-EDED-UNTST-NR     PIC S9(9)   COMP         VALUE ZERO.
           10 WB-EDED-DAT-VON      PIC X(26)                VALUE SPACE.
           10 WB-EDED-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-EDED-DAT-BIS      PIC X(10)                VALUE SPACE.
           10 WB-KTY-BETR-TYP      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BETR-TYP      PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
