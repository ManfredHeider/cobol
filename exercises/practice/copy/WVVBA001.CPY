      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:14:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVBA001                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: BERGHOFER                                        * 00060002
      *    DATUM:    08.03.1994                                       * 00070001
      *    STAND:    08.03.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    31.03.94 17:55:29
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA001                           *
      ******************************************************************
        05  WB-NI-VIVBA001-TAB.
           10 WB-NI-VIVBA001-ELE   PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-VIVBA001            REDEFINES   WB-NI-VIVBA001-TAB.
           10 WB-NI-ST-NR             PIC S9(04)  COMP.
           10 WB-NI-ST-LAST-UPDATE    PIC S9(04)  COMP.
           10 WB-NI-ST-PRUEFZIFFER    PIC S9(04)  COMP.
           10 WB-NI-ST-FREI           PIC S9(04)  COMP.
           10 WB-NI-ST-BEREINIGUNG    PIC S9(04)  COMP.
           10 WB-NI-KTY-ST-TYP        PIC S9(04)  COMP.
           10 WB-NI-KNR-ST-TYP        PIC S9(04)  COMP.
           10 WB-NI-KTY-GESELL-TYP    PIC S9(04)  COMP.
           10 WB-NI-KNR-GESELL-TYP    PIC S9(04)  COMP.
        05  WB-VIVBA001.
           10 WB-ST-NR                PIC S9(9)   COMP      VALUE ZERO.
           10 WB-ST-LAST-UPDATE       PIC X(26)             VALUE SPACE.
           10 WB-ST-PRUEFZIFFER       PIC X(1)              VALUE SPACE.
           10 WB-ST-FREI              PIC X(1)              VALUE SPACE.
           10 WB-ST-BEREINIGUNG       PIC X(1)              VALUE SPACE.
           10 WB-KTY-ST-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-ST-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KTY-GESELL-TYP       PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-GESELL-TYP       PIC S9(4)   COMP      VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
