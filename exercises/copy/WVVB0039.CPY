      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0039.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0039.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:25:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0039                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GRUPPE AS                                        * 00060002
      *    DATUM:    06.10.1994                                       * 00070001
      *    STAND:    06.10.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    29.10.94 10:25:59
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0039                           *
      ******************************************************************
        05  WB-NI-VIVB0039-TAB.
           10 WB-NI-VIVB0039-ELE     PIC S9(04) COMP  OCCURS 13 TIMES.
        05  WB-NI-VIVB0039         REDEFINES  WB-NI-VIVB0039-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-LF-MONAT         PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-PROD-GEB-GES PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-GEB-GES PIC S9(4) USAGE COMP.
           10 WB-NI-LF-BQF-EG        PIC S9(4) USAGE COMP.
           10 WB-NI-LF-BQF-GS        PIC S9(4) USAGE COMP.
           10 WB-NI-LF-BQF-GR        PIC S9(4) USAGE COMP.
           10 WB-NI-LF-DYNQ-EG       PIC S9(4) USAGE COMP.
           10 WB-NI-LF-DYNQ-GS       PIC S9(4) USAGE COMP.
           10 WB-NI-LF-DYNQ-GR       PIC S9(4) USAGE COMP.
           10 WB-NI-LF-NEQF-EG       PIC S9(4) USAGE COMP.
           10 WB-NI-LF-NEQF-GS       PIC S9(4) USAGE COMP.
           10 WB-NI-LF-NEQF-GR       PIC S9(4) USAGE COMP.
        05  WB-VIVB0039.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-LF-MONAT            PIC X(10).
           10 WB-KNR-PROD-GEB-GES    PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-GEB-GES    PIC S9(4) USAGE COMP.
           10 WB-LF-BQF-EG           PIC S999V99 USAGE COMP-3.
           10 WB-LF-BQF-GS           PIC S999V99 USAGE COMP-3.
           10 WB-LF-BQF-GR           PIC S999V99 USAGE COMP-3.
           10 WB-LF-DYNQ-EG          PIC S999V99 USAGE COMP-3.
           10 WB-LF-DYNQ-GS          PIC S999V99 USAGE COMP-3.
           10 WB-LF-DYNQ-GR          PIC S999V99 USAGE COMP-3.
           10 WB-LF-NEQF-EG          PIC S999V99 USAGE COMP-3.
           10 WB-LF-NEQF-GS          PIC S999V99 USAGE COMP-3.
           10 WB-LF-NEQF-GR          PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
