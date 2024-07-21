      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:17:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0001                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0001                           *
      ******************************************************************
        05  WB-NI-VIVB0001-TAB.
           10 WB-NI-VIVB0001-ELE   PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  WB-NI-VIVB0001            REDEFINES   WB-NI-VIVB0001-TAB.
           10 WB-NI-ST-NR             PIC S9(04)  COMP.
           10 WB-NI-ST-LAST-UPDATE    PIC S9(04)  COMP.
           10 WB-NI-ST-PRUEFZIFFER    PIC S9(04)  COMP.
           10 WB-NI-ST-FREI           PIC S9(04)  COMP.
           10 WB-NI-ST-BEREINIGUNG    PIC S9(04)  COMP.
           10 WB-NI-KTY-ST-TYP        PIC S9(04)  COMP.
           10 WB-NI-KNR-ST-TYP        PIC S9(04)  COMP.
        05  WB-VIVB0001.
           10 WB-ST-NR                PIC S9(9)   COMP      VALUE ZERO.
           10 WB-ST-LAST-UPDATE       PIC X(26)             VALUE SPACE.
           10 WB-ST-PRUEFZIFFER       PIC X(1)              VALUE SPACE.
           10 WB-ST-FREI              PIC X(1)              VALUE SPACE.
           10 WB-ST-BEREINIGUNG       PIC X(1)              VALUE SPACE.
           10 WB-KTY-ST-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-ST-TYP           PIC S9(4)   COMP      VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
