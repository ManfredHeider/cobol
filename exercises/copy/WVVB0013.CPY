      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:19:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0013                                      * 00022005
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0013                           *
      ******************************************************************
        05  WB-NI-VIVB0013-TAB.
           10  WB-NI-VIVB0013-ELE  PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-VIVB0013           REDEFINES   WB-NI-VIVB0013-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-AV-DAT-VON       PIC S9(04)  COMP.
           10 WB-NI-AV-LAST-UPDATE   PIC S9(04)  COMP.
           10 WB-NI-KTY-AV-TYP       PIC S9(04)  COMP.
           10 WB-NI-KNR-AV-TYP       PIC S9(04)  COMP.
           10 WB-NI-AV-VERTRAGSDAT   PIC S9(04)  COMP.
           10 WB-NI-AV-DAT-BIS       PIC S9(04)  COMP.
           10 WB-NI-AV-UMSCHREIB-DAT PIC S9(04)  COMP.
           10 WB-NI-AV-UMSCHREIB-PTN PIC S9(04)  COMP.
        05  WB-VIVB0013.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-AV-DAT-VON        PIC X(26)                VALUE SPACE.
           10 WB-AV-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KTY-AV-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-AV-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-AV-VERTRAGSDAT    PIC X(10)                VALUE SPACE.
           10 WB-AV-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-AV-UMSCHREIB-DAT  PIC X(10)                VALUE SPACE.
           10 WB-AV-UMSCHREIB-PTN  PIC X(40)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
