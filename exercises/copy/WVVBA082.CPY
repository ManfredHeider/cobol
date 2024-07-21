      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA082.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WVVBA082.CPv  $
      *
      *   Rev 2.1   Jul 25 2003 08:49:12   t13ae27
      *Erweiterung wegen OLB-2003
      *
      *   Rev 2.0   Jun 26 2003 08:26:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA082                           *
      ******************************************************************
        05  WB-NI-VIVBA082-TAB.
           10 WB-NI-VIVBA082-ELE    PIC S9(04) BINARY OCCURS  29 TIMES.
        05  WB-NI-VIVBA082            REDEFINES   WB-NI-VIVBA082-TAB.
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
        05 WB-VIVBA082.
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
           10 WB-LIST-BETRAG-1              PIC S9(9)V99 COMP-3.
           10 WB-LIST-BETRAG-2              PIC S9(9)V99 COMP-3.
           10 WB-LIST-BETRAG-3              PIC S9(9)V99 COMP-3.
           10 WB-LIST-BETRAG-4              PIC S9(9)V99 COMP-3.
           10 WB-LIST-BETRAG-5              PIC S9(9)V99 COMP-3.
           10 WB-LIST-SUM-BETRAG            PIC S9(9)V99 COMP-3.
           10 WB-LIST-PROZSATZ              PIC S9(5)V9(2) USAGE COMP-3.
           10 WB-LIST-BEMERKUNG             PIC X(20).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************
