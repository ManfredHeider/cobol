      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0097.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0097.CPv  $
      *
      *   Rev 2.1   Mar 27 2006 08:15:24   T13AE27
      *AAS00602: Jahres-Wettbewerb 2006 DVAG Wien
      *
      *   Rev 2.0   Mar 03 2006 14:47:52   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0097                           *
      ******************************************************************
        05  WB-NI-VIVB0097-TAB.
           10 WB-NI-VIVB0097-ELE   PIC S9(04)  BINARY OCCURS 06 TIMES.
        05  WB-NI-VIVB0097         REDEFINES   WB-NI-VIVB0097-TAB.
           10 WB-NI-WBW01-MA-ST-NR         PIC S9(04)       BINARY.
           10 WB-NI-WBW01-MONAT            PIC S9(04)       BINARY.
           10 WB-NI-WBW01-VERTRAG-NR       PIC S9(04)       BINARY.
           10 WB-NI-WBW01-GES-NR           PIC S9(04)       BINARY.
           10 WB-NI-WBW01-PRODUKT-NR       PIC S9(04)       BINARY.
           10 WB-NI-WBW01-STORNO-SCHL      PIC S9(04)       BINARY.

        05  WB-VIVB0097.
           10 WB-WBW01-MA-ST-NR            PIC S9(009)       BINARY.
           10 WB-WBW01-MONAT               PIC S9(004)       BINARY.
           10 WB-WBW01-VERTRAG-NR          PIC S9(004)       BINARY.
           10 WB-WBW01-GES-NR              PIC S9(004)       BINARY.
           10 WB-WBW01-PRODUKT-NR          PIC S9(004)       BINARY.
           10 WB-WBW01-STORNO-SCHL         PIC S9(004)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 06      *
      ******************************************************************
