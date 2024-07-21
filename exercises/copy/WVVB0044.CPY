      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0044.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0044.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:26:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0044                                      * 00022008
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0044                           *
      ******************************************************************
        05  WB-NI-VIVB0044-TAB.
           10  WB-NI-VIVB0044-ELE  PIC S9(04)  COMP   OCCURS  4  TIMES.
        05  WB-NI-VIVB0044         REDEFINES   WB-NI-VIVB0044-TAB.
           10 WB-NI-KTY-SSTS-TYP   PIC S9(04)  COMP.
           10 WB-NI-KNR-SSTS-TYP   PIC S9(04)  COMP.
           10 WB-NI-MS-NR          PIC S9(04)  COMP.
           10 WB-NI-VBP-NR         PIC S9(04)  COMP.
        05  WB-VIVB0044.
           10 WB-KTY-SSTS-TYP      PIC S9(4)  COMP          VALUE ZERO.
           10 WB-KNR-SSTS-TYP      PIC S9(4)  COMP          VALUE ZERO.
           10 WB-MS-NR             PIC S9(4)  COMP          VALUE ZERO.
           10 WB-VBP-NR            PIC S9(4)  COMP          VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
