      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0038.CPv  $
      *
      *   Rev 2.1   Feb 11 2005 11:41:00   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Feb 27 1999 15:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:25:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: WVVB0038                                      * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 
      *    DATUM:    22.04.1993                                       * 
      *    STAND:    22.04.1993                                       * 
      *    --------------------------------------------------------   * 
      *    VERSION:  001                                              * 
      *    Version:  003 Verwendung der Tab. + COPY für PARTEH
      ***************************************************************** 
      *                                                                 
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  WB-VIVB0038                       *
      ******************************************************************
        05  WB-NI-VIVB0038-TAB.
           10  WB-NI-VIVB0038-ELE  PIC S9(04)  COMP   OCCURS  10 TIMES.
        05  WB-NI-VIVB0038           REDEFINES   WB-NI-VIVB0038-TAB.
           10 WB-NI-PARTEH-BETR        PIC S9(04)  COMP.
           10 WB-NI-PARTEH-UNTST       PIC S9(04)  COMP.
           10 WB-NI-PARTEH-MONAT       PIC S9(04)  COMP.
           10 WB-NI-KNR-WB-TYP         PIC S9(04)  COMP.
           10 WB-NI-KNR-WERTUNGS-TYP   PIC S9(04)  COMP.
           10 WB-NI-KTY-WB-TYP         PIC S9(04)  COMP.
           10 WB-NI-KTY-WERTUNGS-TYP   PIC S9(04)  COMP.
           10 WB-NI-PARTEH-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-PARTEH-EH          PIC S9(04)  COMP.
           10 WB-NI-PARTEH-EBENE       PIC S9(04)  COMP.
        05  WB-VIVB0038.
           10 WB-PARTEH-BETR           PIC S9(9)   COMP     VALUE ZERO.
           10 WB-PARTEH-UNTST          PIC S9(9)   COMP     VALUE ZERO.
           10 WB-PARTEH-MONAT          PIC X(10)            VALUE SPACE.
           10 WB-KNR-WB-TYP            PIC S9(4)   COMP     VALUE ZERO.
           10 WB-KNR-WERTUNGS-TYP      PIC S9(4)   COMP     VALUE ZERO.
           10 WB-KTY-WB-TYP            PIC S9(4)   COMP     VALUE ZERO.
           10 WB-KTY-WERTUNGS-TYP      PIC S9(4)   COMP     VALUE ZERO.
           10 WB-PARTEH-LAST-UPDATE    PIC X(26)            VALUE SPACE.
           10 WB-PARTEH-EH             PIC S9(9)V9 COMP-3   VALUE ZERO.
           10 WB-PARTEH-EBENE          PIC S9(4)   COMP     VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
