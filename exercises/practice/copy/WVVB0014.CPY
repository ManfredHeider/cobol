      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0014.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0014.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:19:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0014                                      * 00022005
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0014                           *
      ******************************************************************
        05  WB-NI-VIVB0014-TAB.
           10  WB-NI-VIVB0014-ELE  PIC S9(04)  COMP   OCCURS  13 TIMES.
        05  WB-NI-VIVB0014          REDEFINES   WB-NI-VIVB0014-TAB.
           10 WB-NI-MAN-BEZUGS-NR   PIC S9(04)  COMP.
           10 WB-NI-MAN-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-MAN-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-MAN-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-MAN-GEBURTSDAT  PIC S9(04)  COMP.
           10 WB-NI-KTY-ANREDE      PIC S9(04)  COMP.
           10 WB-NI-KNR-ANREDE      PIC S9(04)  COMP.
           10 WB-NI-MAN-TITEL-ADEL  PIC S9(04)  COMP.
           10 WB-NI-MAN-TITEL-AKAD  PIC S9(04)  COMP.
           10 WB-NI-MAN-NAME        PIC S9(04)  COMP.
           10 WB-NI-MAN-VORNAME     PIC S9(04)  COMP.
        05  WB-VIVB0014.
           10 WB-MAN-BEZUGS-NR     PIC S9(9)   COMP         VALUE ZERO.
           10 WB-MAN-DAT-VON       PIC X(26)                VALUE SPACE.
           10 WB-MAN-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-MAN-DAT-BIS       PIC X(10)                VALUE SPACE.
           10 WB-KTY-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MAN-GEBURTSDAT    PIC X(10)                VALUE SPACE.
           10 WB-KTY-ANREDE        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-ANREDE        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MAN-TITEL-ADEL    PIC X(10)                VALUE SPACE.
           10 WB-MAN-TITEL-AKAD    PIC X(10)                VALUE SPACE.
           10 WB-MAN-NAME          PIC X(40)                VALUE SPACE.
           10 WB-MAN-VORNAME       PIC X(20)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
