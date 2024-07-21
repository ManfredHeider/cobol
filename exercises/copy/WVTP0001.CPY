      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  26.02.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVTP0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVTP0001.CPv  $
      *
      *   Rev 2.2   Nov 04 2005 11:56:36   T13AE27
      *AAS00572: Siegeradler 2005
      *
      *   Rev 2.1   Oct 29 2003 15:41:06   t13ae27
      *neu
      *
      *   Rev 2.0   Feb 26 2003 12:11:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBTP0001                           *
      ******************************************************************
        05 WB-NI-TBTP0001-TAB.
           10 WB-NI-TBTP0001-ELE    PIC S9(04) BINARY OCCURS 15  TIMES.
        05  WB-NI-TBTP0001            REDEFINES   WB-NI-TBTP0001-TAB.
            10 WB-NI-TMP-POS-VOR-ABZPRZ   PIC S9(04)   COMP.
            10 WB-NI-TMP-VBNR             PIC S9(04)   COMP.
            10 WB-NI-TMP-E-VOR-ABZPRZ     PIC S9(04)   COMP.
            10 WB-NI-TMP-STUFE-AKT        PIC S9(04)   COMP.
            10 WB-NI-TMP-MAN-TITEL-ADEL   PIC S9(04)   COMP.
            10 WB-NI-TMP-MAN-TITEL-AKAD   PIC S9(04)   COMP.
            10 WB-NI-TMP-MAN-NAME         PIC S9(04)   COMP.
            10 WB-NI-TMP-MAN-VORNAME      PIC S9(04)   COMP.
            10 WB-NI-TMP-DIR-NR           PIC S9(04)   COMP.
            10 WB-NI-TMP-EQF-12-MON       PIC S9(04)   COMP.
            10 WB-NI-TMP-E-NACH-ABZPRZ    PIC S9(04)   COMP.
            10 WB-NI-TMP-STUFE-WERTUNG    PIC S9(04)   COMP.
            10 WB-NI-TMP-POS-WERTUNG-V    PIC S9(04)   COMP.
            10 WB-NI-TMP-POS-WERTUNG-N    PIC S9(04)   COMP.
            10 WB-NI-TMP-PROZ             PIC S9(04)   COMP.

        05  WB-TBTP0001.
            10 WB-TMP-POS-VOR-ABZPRZ      PIC S9(04)   COMP.
            10 WB-TMP-VBNR                PIC S9(09)   COMP.
            10 WB-TMP-E-VOR-ABZPRZ        PIC S9(09)V9 COMP-3.
            10 WB-TMP-STUFE-AKT           PIC S9(04)   COMP.
            10 WB-TMP-MAN-VORNAME         PIC  X(20).
            10 WB-TMP-MAN-NAME            PIC  X(40).
            10 WB-TMP-MAN-TITEL-ADEL      PIC  X(10).
            10 WB-TMP-MAN-TITEL-AKAD      PIC  X(10).
            10 WB-TMP-DIR-NR              PIC S9(04)   COMP.
            10 WB-TMP-EQF-12-MON          PIC S9(03)V99 COMP-3.
            10 WB-TMP-E-NACH-ABZPRZ       PIC S9(09)V9 COMP-3.
            10 WB-TMP-STUFE-WERTUNG       PIC S9(04)   COMP.
            10 WB-TMP-POS-WERTUNG-V       PIC S9(04)   COMP.
            10 WB-TMP-POS-WERTUNG-N       PIC S9(04)   COMP.
            10 WB-TMP-PROZ                PIC S9(03)V99 COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
