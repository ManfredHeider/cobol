      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:18:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0006                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0006                           *
      ******************************************************************
        05  WB-NI-VIVB0006-TAB.
           10  WB-NI-VIVB0006-ELE    PIC S9(04)  COMP   OCCURS 11 TIMES.
        05  WB-NI-VIVB0006           REDEFINES   WB-NI-VIVB0006-TAB.
           10 WB-NI-DIR-NR           PIC S9(04)  COMP.
           10 WB-NI-DIR-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-KTY-DIR-TYP      PIC S9(04)  COMP.
           10 WB-NI-KNR-DIR-TYP      PIC S9(04)  COMP.
           10 WB-NI-KTY-REGION       PIC S9(04)  COMP.
           10 WB-NI-KNR-REGION       PIC S9(04)  COMP.
           10 WB-NI-KTY-RESERV-TYP   PIC S9(04)  COMP.
           10 WB-NI-KNR-RESERV-TYP   PIC S9(04)  COMP.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-DIR-NAME         PIC S9(04)  COMP.
           10 WB-NI-DIR-INFORMATION  PIC S9(04)  COMP.
        05  WB-VIVB0006.
           10 WB-DIR-NR            PIC S9(4)   COMP         VALUE ZERO.
           10 WB-DIR-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-KTY-DIR-TYP       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-DIR-TYP       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-REGION        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-REGION        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-RESERV-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-RESERV-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-DIR-NAME          PIC X(40)                VALUE SPACE.
           10 WB-DIR-INFORMATION   PIC X(150)               VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
