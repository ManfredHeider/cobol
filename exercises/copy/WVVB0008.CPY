      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:18:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0008                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0008                           *
      ******************************************************************
        05  WB-NI-VIVB0008-TAB.
           10  WB-NI-VIVB0008-ELE  PIC S9(04)  COMP   OCCURS  10 TIMES.
        05  WB-NI-VIVB0008           REDEFINES   WB-NI-VIVB0008-TAB.
           10 WB-NI-MS-NR            PIC S9(04)  COMP.
           10 WB-NI-BS-NR            PIC S9(04)  COMP.
           10 WB-NI-MSBS-EH-EIG-GES  PIC S9(04)  COMP.
           10 WB-NI-MSBS-EH-EIG-PZTR PIC S9(04)  COMP.
           10 WB-NI-MSBS-ZEITRAUM    PIC S9(04)  COMP.
           10 WB-NI-MSBS-EQF-EIG     PIC S9(04)  COMP.
           10 WB-NI-MSBS-PROV-STF    PIC S9(04)  COMP.
           10 WB-NI-MSBS-LAUFZEIT    PIC S9(04)  COMP.
           10 WB-NI-MSBS-GUELTIG-BIS PIC S9(04)  COMP.
           10 WB-NI-MSBS-BEZ         PIC S9(04)  COMP.
        05  WB-VIVB0008.
           10 WB-MS-NR             PIC S9(4)   COMP         VALUE ZERO.
           10 WB-BS-NR             PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MSBS-EH-EIG-GES   PIC S9(09)V9  COMP-3     VALUE ZERO.
           10 WB-MSBS-EH-EIG-PZTR  PIC S9(09)V9  COMP-3     VALUE ZERO.
           10 WB-MSBS-ZEITRAUM     PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MSBS-EQF-EIG      PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-MSBS-PROV-STF     PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-MSBS-LAUFZEIT     PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MSBS-GUELTIG-BIS  PIC X(10)                VALUE SPACE.
           10 WB-MSBS-BEZ          PIC X(40)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
