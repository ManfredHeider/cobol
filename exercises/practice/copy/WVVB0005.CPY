      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0005.CPv  $
      *
      *   Rev 2.1   Jan 17 2011 13:19:16   t13ae30
      *AVB01125: Erweiterung der Tabelle RF-T 
      *
      *   Rev 2.0   Feb 27 1999 15:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:17:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: WVVB0005                                      * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 
      *    DATUM:    22.04.1993                                       * 
      *    STAND:    22.04.1993                                       * 
      *    --------------------------------------------------------   * 
      *    VERSION:  001                                              * 
      ***************************************************************** 
      *                                                                 
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0005                           *
      ******************************************************************
        05  WB-NI-VIVB0005-TAB.
           10  WB-NI-VIVB0005-ELE  PIC S9(04)  COMP   OCCURS 11 TIMES.
        05  WB-NI-VIVB0005          REDEFINES   WB-NI-VIVB0005-TAB.
           10 WB-NI-RFT-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-RFT-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-MA-ST-NR        PIC S9(04)  COMP.
           10 WB-NI-RFT-PROV-STZ-AP PIC S9(04)  COMP.
           10 WB-NI-RFT-PROV-STZ-GP PIC S9(04)  COMP.
           10 WB-NI-RFT-PROZ-STZ    PIC S9(04)  COMP.
           10 WB-NI-RFT-RCK-STZ     PIC S9(04)  COMP.
           10 WB-NI-RFT-RCK-STZ-DAT PIC S9(04)  COMP.
           10 WB-NI-RFT-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-RFT-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-RFT-ABLAUF-DAT  PIC S9(04)  COMP.
        05  WB-VIVB0005.
           10 WB-RFT-ST-NR         PIC S9(9)   COMP         VALUE ZERO.
           10 WB-RFT-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-RFT-PROV-STZ-AP   PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-RFT-PROV-STZ-GP   PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-RFT-PROZ-STZ      PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-RFT-RCK-STZ       PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-RFT-RCK-STZ-DAT   PIC X(10)                VALUE SPACE.
           10 WB-RFT-DAT-VON       PIC X(10)                VALUE SPACE.
           10 WB-RFT-DAT-BIS       PIC X(10)                VALUE SPACE.
           10 WB-RFT-ABLAUF-DAT    PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
