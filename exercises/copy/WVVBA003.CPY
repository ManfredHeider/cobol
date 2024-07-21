      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA003.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 10:25:18   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - Tabelle
      *
      *   Rev 2.0   Mar 17 2009 10:21:04   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: WVVBA003                                      * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: S. Walb                                          * 
      *    DATUM:    17.03.2009                                       * 
      *    STAND:    17.03.2009                                       * 
      *    --------------------------------------------------------   * 
      *    VERSION:  001                                              * 
      ***************************************************************** 
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01029!17.03.09!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                                 
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA003                           *
      ******************************************************************
        05  WB-NI-VIVBA003-TAB.
           10  WB-NI-VIVBA003-ELE  PIC S9(04) COMP   OCCURS 13  TIMES.
        05  WB-NI-VIVBA003         REDEFINES  WB-NI-VIVBA003-TAB.
           10 WB-NI-ED-ST-NR       PIC S9(04) COMP.
           10 WB-NI-ED-LAST-UPDATE PIC S9(04) COMP.
           10 WB-NI-MA-ST-NR       PIC S9(04) COMP.
           10 WB-NI-ED-RCK-STZ     PIC S9(04) COMP.
           10 WB-NI-ED-RCK-STZ-DAT PIC S9(04) COMP.
           10 WB-NI-ED-BEZ         PIC S9(04) COMP.
           10 WB-NI-ED-GENERATION  PIC S9(04) COMP.
           10 WB-NI-ED-ZWEIG       PIC S9(04) COMP.
           10 WB-NI-ED-ZWEIG-VOM   PIC S9(04) COMP.
           10 WB-NI-ED-PROV1       PIC S9(04) COMP.
           10 WB-NI-ED-PROV-DAT1   PIC S9(04) COMP.
           10 WB-NI-ED-PROV2       PIC S9(04) COMP.
           10 WB-NI-ED-PROV-DAT2   PIC S9(04) COMP.
        05  WB-VIVBA003.
           10 WB-ED-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-ED-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-ED-RCK-STZ        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-ED-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-ED-BEZ            PIC X(40)                VALUE SPACE.
           10 WB-ED-GENERATION     PIC S9(04)  USAGE COMP   VALUE ZERO.
           10 WB-ED-ZWEIG          PIC S9(09)  USAGE COMP   VALUE ZERO.
           10 WB-ED-ZWEIG-VOM      PIC X(10)                VALUE SPACE.
           10 WB-ED-PROV1          PIC S999V99 USAGE COMP-3 VALUE ZERO.
           10 WB-ED-PROV-DAT1      PIC X(10)                VALUE SPACE.
           10 WB-ED-PROV2          PIC S999V99 USAGE COMP-3 VALUE ZERO.
           10 WB-ED-PROV-DAT2      PIC X(10)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
