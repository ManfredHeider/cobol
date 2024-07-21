      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.12.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB082.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBB082.CPv  $
      *
      *   Rev 2.2   Mar 11 2004 12:05:18   t13ae27
      *Scharfschaltung
      *
      *   Rev 2.1   Dec 18 2003 12:00:08   t13ex22
      *AAS00443: Erweiterung für Wettbewerb 2004
      *
      *   Rev 2.0   Dec 08 2003 15:10:50   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  JANUAR 2001
      *            KOPF AUS:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 2001 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   WVVB0082.CPY  $
      *    $REVISION:   2.3  $
      *
      *    $LOG:   L:\MF\CPY\WVVB0082.CPV  $
      *
      *   REV 2.3   31 MAY 2001 10:41:24   T13AE27
      *NEU
      *
      *   REV 2.2   18 MAY 2001 15:49:50   T13AE30
      *
      *
      *   REV 2.1   17 MAY 2001 10:56:00   T13AE30
      *AAS00274 - PIN FÜR VB-PORTAL
      *
      *   REV 2.0   20 FEB 2001 13:29:34   P13SP16
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0082                           *
      ******************************************************************
        05  WB-NI-VIVBB082-TAB.
           10 WB-NI-VIVBB082-ELE   PIC S9(04) BINARY OCCURS 34 TIMES.

        05   WB-NI-VIVBB082            REDEFINES   WB-NI-VIVBB082-TAB.
           10 WB-NI-MA-ST-NR                PIC S9(04)  BINARY.
           10 WB-NI-LIST-MONAT              PIC S9(04)  BINARY.
           10 WB-NI-KTY-WB-TYP              PIC S9(04)  BINARY.
           10 WB-NI-KNR-WB-TYP              PIC S9(04)  BINARY.
           10 WB-NI-KTY-WERTUNGS-TYP        PIC S9(04)  BINARY.
           10 WB-NI-KNR-WERTUNGS-TYP        PIC S9(04)  BINARY.
           10 WB-NI-LIST-STUF-BEZ           PIC S9(04)  BINARY.
           10 WB-NI-LIST-LAST-UPDATE        PIC S9(04)  BINARY.
           10 WB-NI-DIR-NR                  PIC S9(04)  BINARY.
           10 WB-NI-LIST-NAME               PIC S9(04)  BINARY.
           10 WB-NI-LIST-VORNAME            PIC S9(04)  BINARY.
           10 WB-NI-LIST-RANG               PIC S9(04)  BINARY.
           10 WB-NI-LIST-GEWINNER-KZ        PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-1               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-2               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-3               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-4               PIC S9(04)  BINARY.
           10 WB-NI-LIST-EH-5               PIC S9(04)  BINARY.
           10 WB-NI-LIST-SUM-EH             PIC S9(04)  BINARY.
           10 WB-NI-KTY-REGION              PIC S9(04)  BINARY.
           10 WB-NI-KNR-REGION              PIC S9(04)  BINARY.
           10 WB-NI-LIST-BETRAG-1           PIC S9(04)  BINARY.
           10 WB-NI-LIST-BETRAG-2           PIC S9(04)  BINARY.
           10 WB-NI-LIST-BETRAG-3           PIC S9(04)  BINARY.
           10 WB-NI-LIST-BETRAG-4           PIC S9(04)  BINARY.
           10 WB-NI-LIST-BETRAG-5           PIC S9(04)  BINARY.
           10 WB-NI-LIST-SUM-BETRAG         PIC S9(04)  BINARY.
           10 WB-NI-LIST-PROZSATZ           PIC S9(04)  BINARY.
           10 WB-NI-LIST-BEMERKUNG          PIC S9(04)  BINARY.
           10 WB-NI-KTY-LIST-TYP            PIC S9(04)  BINARY.
           10 WB-NI-KNR-LIST-TYP            PIC S9(04)  BINARY.
           10 WB-NI-KTY-GESELL-TYP          PIC S9(04)  BINARY.
           10 WB-NI-KNR-GESELL-TYP          PIC S9(04)  BINARY.
           10 WB-NI-LIST-WERTUNGSSTUFE      PIC S9(04)  BINARY.
        05 WB-VIVBB082.
           10 WB-MA-ST-NR                   PIC S9(9)   USAGE COMP.
           10 WB-LIST-MONAT                 PIC X(10).
           10 WB-KTY-WB-TYP                 PIC S9(4)   USAGE COMP.
           10 WB-KNR-WB-TYP                 PIC S9(4)   USAGE COMP.
           10 WB-KTY-WERTUNGS-TYP           PIC S9(4)   USAGE COMP.
           10 WB-KNR-WERTUNGS-TYP           PIC S9(4)   USAGE COMP.
           10 WB-LIST-STUF-BEZ              PIC X(10).
           10 WB-LIST-LAST-UPDATE           PIC X(26).
           10 WB-DIR-NR                     PIC S9(4)   USAGE COMP.
           10 WB-LIST-NAME                  PIC X(25).
           10 WB-LIST-VORNAME               PIC X(20).
           10 WB-LIST-RANG                  PIC S9(9)   USAGE COMP.
           10 WB-LIST-GEWINNER-KZ           PIC X(01).
           10 WB-LIST-EH-1                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-2                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-3                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-4                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-EH-5                  PIC S9(9)V9 COMP-3.
           10 WB-LIST-SUM-EH                PIC S9(9)V9 COMP-3.
           10 WB-KTY-REGION                 PIC S9(4)   USAGE COMP.
           10 WB-KNR-REGION                 PIC S9(4)   USAGE COMP.
           10 WB-LIST-BETRAG-1              PIC S9(9)V9(2) COMP-3.
           10 WB-LIST-BETRAG-2              PIC S9(9)V9(2) COMP-3.
           10 WB-LIST-BETRAG-3              PIC S9(9)V9(2) COMP-3.
           10 WB-LIST-BETRAG-4              PIC S9(9)V9(2) COMP-3.
           10 WB-LIST-BETRAG-5              PIC S9(9)V9(2) COMP-3.
           10 WB-LIST-SUM-BETRAG            PIC S9(9)V9(2) COMP-3.
           10 WB-LIST-PROZSATZ              PIC S9(5)V9(2) COMP-3.
           10 WB-LIST-BEMERKUNG             PIC X(20).
           10 WB-KTY-LIST-TYP               PIC S9(4) COMP.
           10 WB-KNR-LIST-TYP               PIC S9(4) COMP.
           10 WB-KTY-GESELL-TYP             PIC S9(4) COMP.
           10 WB-KNR-GESELL-TYP             PIC S9(4) COMP.
           10 WB-LIST-WERTUNGSSTUFE         PIC S9(4) COMP.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************

