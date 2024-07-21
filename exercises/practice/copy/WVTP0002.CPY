      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  26.02.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVTP0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WVTP0002.CPv  $
      *
      *   Rev 2.1   Oct 29 2003 15:41:18   t13ae27
      *neu
      *
      *   Rev 2.0   Feb 26 2003 12:11:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBTP0002                           *
      ******************************************************************
        05 WB-NI-TBTP0002-TAB.
           10 WB-NI-TBTP0002-ELE    PIC S9(04) BINARY OCCURS 11  TIMES.
        05  WB-NI-TBTP0002            REDEFINES   WB-NI-TBTP0002-TAB.
            10 WB-NI-TMP-KZ               PIC S9(04)   COMP.
            10 WB-NI-TMP-VBNR             PIC S9(04)   COMP.
            10 WB-NI-TMP-EQF-12-MON       PIC S9(04)   COMP.
            10 WB-NI-TMP-BES              PIC S9(04)   COMP.
            10 WB-NI-TMP-UNTST-NR         PIC S9(04)   COMP.
            10 WB-NI-TMP-UNTST-DAT-VON    PIC S9(04)   COMP.
            10 WB-NI-TMP-UNTST-DAT-BIS    PIC S9(04)   COMP.
            10 WB-NI-TMP-E-BASIS-ABZPRZ   PIC S9(04)   COMP.
            10 WB-NI-TMP-E-BETR-BASIS     PIC S9(04)   COMP.
            10 WB-NI-TMP-E-UNTST          PIC S9(04)   COMP.
            10 WB-NI-TMP-DATUM            PIC S9(04)   COMP.

        05  WB-TBTP0002.
            10 WB-TMP-KZ                  PIC  X(03).
            10 WB-TMP-VBNR                PIC S9(09)   COMP.
            10 WB-TMP-EQF-12-MON          PIC S9(03)V99 COMP-3.
            10 WB-TMP-BES                 PIC S9(04)   COMP.
            10 WB-TMP-UNTST-NR            PIC S9(09)   COMP.
            10 WB-TMP-UNTST-DAT-VON       PIC  X(10).
            10 WB-TMP-UNTST-DAT-BIS       PIC  X(10).
            10 WB-TMP-E-BASIS-ABZPRZ      PIC S9(09)V9 COMP-3.
            10 WB-TMP-E-BETR-BASIS        PIC S9(09)V9 COMP-3.
            10 WB-TMP-E-UNTST             PIC S9(09)V9 COMP-3.
            10 WB-TMP-DATUM               PIC  X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
