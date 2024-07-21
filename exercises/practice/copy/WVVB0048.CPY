      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0048.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0048.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:26:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0048                                      * 00022008
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0048                           *
      ******************************************************************
        05  WB-NI-VIVB0048-TAB.
           10  WB-NI-VIVB0048-ELE  PIC S9(04)  COMP   OCCURS  2  TIMES.
        05  WB-NI-VIVB0048         REDEFINES   WB-NI-VIVB0048-TAB.
           10 WB-NI-MSA-AKTUELL-STF PIC S9(4) USAGE COMP.
           10 WB-NI-MSA-NEU-STF    PIC S9(4) USAGE COMP.
        05  WB-VIVB0048.
           10 WB-MSA-AKTUELL-STF   PIC S9(4)  COMP          VALUE ZERO.
           10 WB-MSA-NEU-STF       PIC S9(4)  COMP          VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
