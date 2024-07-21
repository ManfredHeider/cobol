      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:20:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0015                                      * 00022005
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0015                           *
      ******************************************************************
        05  WB-NI-VIVB0015-TAB.
           10  WB-NI-VIVB0015-ELE  PIC S9(04)  COMP   OCCURS  24 TIMES.
        05  WB-NI-VIVB0015           REDEFINES   WB-NI-VIVB0015-TAB.
           10 WB-NI-MAAN-BEZUGS-NR   PIC S9(04)  COMP.
           10 WB-NI-KTY-MAAN-TYP     PIC S9(04)  COMP.
           10 WB-NI-KNR-MAAN-TYP     PIC S9(04)  COMP.
           10 WB-NI-MAAN-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-MAAN-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-MAAN-STRASSE     PIC S9(04)  COMP.
           10 WB-NI-MAAN-PLZ-AKTUELL PIC S9(04)  COMP.
           10 WB-NI-MAAN-PLZ         PIC S9(04)  COMP.
           10 WB-NI-MAAN-ORT         PIC S9(04)  COMP.
           10 WB-NI-MAAN-PLZ-PF      PIC S9(04)  COMP.
           10 WB-NI-MAAN-ORT-PF      PIC S9(04)  COMP.
           10 WB-NI-MAAN-POSTFACH    PIC S9(04)  COMP.
           10 WB-NI-MAAN-WOHNT-BEI   PIC S9(04)  COMP.
           10 WB-NI-MAAN-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-MAAN-TELEFON     PIC S9(04)  COMP.
           10 WB-NI-MAAN-TELEFAX     PIC S9(04)  COMP.
           10 WB-NI-KTY-AUSL         PIC S9(04)  COMP.
           10 WB-NI-KNR-AUSL         PIC S9(04)  COMP.
           10 WB-NI-KTY-GRND         PIC S9(04)  COMP.
           10 WB-NI-KNR-GRND         PIC S9(04)  COMP.
           10 WB-NI-KTY-ZUSTELLUNG   PIC S9(04)  COMP.
           10 WB-NI-KNR-ZUSTELLUNG   PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZUGS-TYP   PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZUGS-TYP   PIC S9(04)  COMP.
        05  WB-VIVB0015.
           10 WB-MAAN-BEZUGS-NR    PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KTY-MAAN-TYP      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-MAAN-TYP      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MAAN-DAT-VON      PIC X(26)                VALUE SPACE.
           10 WB-MAAN-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-MAAN-STRASSE      PIC X(40)                VALUE SPACE.
           10 WB-MAAN-PLZ-AKTUELL  PIC X(9)                 VALUE SPACE.
           10 WB-MAAN-PLZ          PIC X(9)                 VALUE SPACE.
           10 WB-MAAN-ORT          PIC X(40)                VALUE SPACE.
           10 WB-MAAN-PLZ-PF       PIC X(9)                 VALUE SPACE.
           10 WB-MAAN-ORT-PF       PIC X(40)                VALUE SPACE.
           10 WB-MAAN-POSTFACH     PIC X(6)                 VALUE SPACE.
           10 WB-MAAN-WOHNT-BEI    PIC X(40)                VALUE SPACE.
           10 WB-MAAN-DAT-BIS      PIC X(10)                VALUE SPACE.
           10 WB-MAAN-TELEFON      PIC X(14)                VALUE SPACE.
           10 WB-MAAN-TELEFAX      PIC X(14)                VALUE SPACE.
           10 WB-KTY-AUSL          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-AUSL          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-GRND          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-GRND          PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-ZUSTELLUNG    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-ZUSTELLUNG    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BEZUGS-TYP    PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
