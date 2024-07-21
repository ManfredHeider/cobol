      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WVVB0078.CPv  $
      *
      *   Rev 2.1   19 Mar 2001 17:21:10   T13AE30
      *AAS00264- LEISTUNGSPLAN / WETTBEWERB 2001
      *
      *   Rev 2.0   20 Feb 2001 13:29:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0078                           *
      ******************************************************************
        05  WB-NI-VIVB0078-TAB.
           10 WB-NI-VIVB0078-ELE   PIC S9(04)  BINARY OCCURS  9 TIMES.
        05  WB-NI-VIVB0078         REDEFINES   WB-NI-VIVB0078-TAB.
           10 WB-NI-MA-ST-NR               PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP             PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP             PIC S9(04)       BINARY.
           10 WB-NI-KTY-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KNR-WERTUNGS-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KAND-WERTUNGSSTUFE     PIC S9(04)       BINARY.
           10 WB-NI-KAND-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KAND-TAETIG-BIS        PIC S9(04)       BINARY.
           10 WB-NI-KAND-DRUCK-KZ          PIC S9(04)       BINARY.

        05  WB-VIVB0078.
           10 WB-MA-ST-NR                 PIC S9(009)       BINARY.
           10 WB-KTY-WB-TYP               PIC S9(004)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(004)       BINARY.
           10 WB-KTY-WERTUNGS-TYP         PIC S9(004)       BINARY.
           10 WB-KNR-WERTUNGS-TYP         PIC S9(004)       BINARY.
           10 WB-KAND-WERTUNGSSTUFE       PIC S9(004)       BINARY.
           10 WB-KAND-LAST-UPDATE         PIC X(026).
           10 WB-KAND-TAETIG-BIS          PIC X(010).
           10 WB-KAND-DRUCK-KZ            PIC S9(004)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
