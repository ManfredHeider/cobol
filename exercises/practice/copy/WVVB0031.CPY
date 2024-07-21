      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0031.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0031.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:23:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0031                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0031                           *
      ******************************************************************
        05  WB-NI-VIVB0031-TAB.
           10  WB-NI-VIVB0031-ELE  PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-VIVB0031         REDEFINES   WB-NI-VIVB0031-TAB.
           10 WB-NI-PCZ-BEZUGS-NR   PIC S9(04)  COMP.
           10 WB-NI-PCZ-NR          PIC S9(04)  COMP.
           10 WB-NI-PCZ-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-PCZ-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KTY-PCZ-TYP     PIC S9(04)  COMP.
           10 WB-NI-KNR-PCZ-TYP     PIC S9(04)  COMP.
           10 WB-NI-PCZ-KENNZEICHEN PIC S9(04)  COMP.
        05  WB-VIVB0031.
           10 WB-PCZ-BEZUGS-NR     PIC S9(9)   COMP         VALUE ZERO.
           10 WB-PCZ-NR            PIC S9(9)   COMP         VALUE ZERO.
           10 WB-PCZ-DAT-VON       PIC X(10)                VALUE SPACE.
           10 WB-PCZ-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-KTY-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-PCZ-TYP       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-PCZ-TYP       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-PCZ-KENNZEICHEN   PIC X(1)                 VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
