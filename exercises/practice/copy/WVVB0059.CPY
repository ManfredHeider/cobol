      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0059.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0059.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:28:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0059                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GRUPPE AS                                        * 00060002
      *    DATUM:    06.10.1994                                       * 00070001
      *    STAND:    06.10.1994                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    29.10.94 10:26:45
      ***************************************************************** 00090000
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0059                           *
      ******************************************************************
        05  WB-NI-VIVB0059-TAB.
           10 WB-NI-VIVB0059-ELE     PIC S9(04) COMP  OCCURS 17 TIMES.
        05  WB-NI-VIVB0059         REDEFINES  WB-NI-VIVB0059-TAB.
           10 WB-NI-ST-NR            PIC S9(4) USAGE COMP.
           10 WB-NI-AST-MONAT        PIC S9(4) USAGE COMP.
           10 WB-NI-AST-JEQF-EG      PIC S9(4) USAGE COMP.
           10 WB-NI-AST-JEQF-GR      PIC S9(4) USAGE COMP.
           10 WB-NI-AST-JEQF-GS      PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-AB-78-EG  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-AB-78-GR  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-BIS-78-GS PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-FG-ZU-EG  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-FG-ZU-GR  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-FG-AB-EG  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-FG-AB-GR  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-ZU-EG  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-ZU-GR  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-AB-EG  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-EH-VM-AB-GR  PIC S9(4) USAGE COMP.
           10 WB-NI-AST-PROV-RCK-ZFL PIC S9(4) USAGE COMP.
        05  WB-VIVB0059.
           10 WB-ST-NR               PIC S9(9) USAGE COMP.
           10 WB-AST-MONAT           PIC X(10).
           10 WB-AST-JEQF-EG         PIC S999V99 USAGE COMP-3.
           10 WB-AST-JEQF-GR         PIC S999V99 USAGE COMP-3.
           10 WB-AST-JEQF-GS         PIC S999V99 USAGE COMP-3.
           10 WB-AST-EH-AB-78-EG     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-AB-78-GR     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-BIS-78-GS    PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-FG-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-FG-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-FG-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-FG-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-EH-VM-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 WB-AST-PROV-RCK-ZFL    PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
