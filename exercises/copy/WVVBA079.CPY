      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA079.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WVVBA079.CPv  $
      *
      *   Rev 2.1   Jul 25 2003 08:47:56   t13ae27
      *Erweiterung wegen OLB-2003
      *
      *   Rev 2.0   Jun 26 2003 08:26:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA079                           *
      ******************************************************************
        05  WB-NI-VIVBA079-TAB.
           10 WB-NI-VIVBA079-ELE   PIC S9(04)  BINARY OCCURS  24 TIMES.
        05  WB-NI-VIVBA079         REDEFINES   WB-NI-VIVBA079-TAB.
           10 WB-NI-MA-ST-NR           PIC S9(04)       BINARY.
           10 WB-NI-ERG-MONAT          PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KTY-WERTUNGS-TYP   PIC S9(04)       BINARY.
           10 WB-NI-KNR-WERTUNGS-TYP   PIC S9(04)       BINARY.
           10 WB-NI-ERG-WERTUNGSSTUFE  PIC S9(04)       BINARY.
           10 WB-NI-ERG-LAST-UPDATE    PIC S9(04)       BINARY.
           10 WB-NI-ERG-EQF-1          PIC S9(04)       BINARY.
           10 WB-NI-ERG-EQF-2          PIC S9(04)       BINARY.
           10 WB-NI-ERG-SUM-EH         PIC S9(04)       BINARY.
           10 WB-NI-ERG-EH-1           PIC S9(04)       BINARY.
           10 WB-NI-ERG-EH-2           PIC S9(04)       BINARY.
           10 WB-NI-ERG-EH-3           PIC S9(04)       BINARY.
           10 WB-NI-ERG-EH-4           PIC S9(04)       BINARY.
           10 WB-NI-ERG-EH-5           PIC S9(04)       BINARY.
           10 WB-NI-ERG-SUM-BETR       PIC S9(04)       BINARY.
           10 WB-NI-ERG-BETR-1         PIC S9(04)       BINARY.
           10 WB-NI-ERG-BETR-2         PIC S9(04)       BINARY.
           10 WB-NI-ERG-BETR-3         PIC S9(04)       BINARY.
           10 WB-NI-ERG-BETR-4         PIC S9(04)       BINARY.
           10 WB-NI-ERG-BETR-5         PIC S9(04)       BINARY.
           10 WB-NI-ERG-PROZSATZ       PIC S9(04)       BINARY.
           10 WB-NI-ERG-BEMERKUNG      PIC S9(04)       BINARY.

        05  WB-VIVBA079.
           10 WB-MA-ST-NR              PIC S9(9) USAGE COMP.
           10 WB-ERG-MONAT             PIC X(10).
           10 WB-KTY-WB-TYP            PIC S9(4) USAGE COMP.
           10 WB-KNR-WB-TYP            PIC S9(4) USAGE COMP.
           10 WB-KTY-WERTUNGS-TYP      PIC S9(4) USAGE COMP.
           10 WB-KNR-WERTUNGS-TYP      PIC S9(4) USAGE COMP.
           10 WB-ERG-WERTUNGSSTUFE     PIC S9(4) USAGE COMP.
           10 WB-ERG-LAST-UPDATE       PIC X(26).
           10 WB-ERG-EQF-1             PIC S9(3)V9(2) USAGE COMP-3.
           10 WB-ERG-EQF-2             PIC S9(3)V9(2) USAGE COMP-3.
           10 WB-ERG-SUM-EH            PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-ERG-EH-1              PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-ERG-EH-2              PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-ERG-EH-3              PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-ERG-EH-4              PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-ERG-EH-5              PIC S9(9)V9(1) USAGE COMP-3.
           10 WB-ERG-SUM-BETR          PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-ERG-BETR-1            PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-ERG-BETR-2            PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-ERG-BETR-3            PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-ERG-BETR-4            PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-ERG-BETR-5            PIC S9(9)V9(2) USAGE COMP-3.
           10 WB-ERG-PROZSATZ          PIC S9(5)V9(2) USAGE COMP-3.
           10 WB-ERG-BEMERKUNG         PIC X(20).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      *********************************************

