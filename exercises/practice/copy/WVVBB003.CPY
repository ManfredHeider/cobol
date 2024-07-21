      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.01.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB003.CPv  $
      *
      *   Rev 2.1   Apr 30 2010 12:11:20   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - Tabellen
      *
      *   Rev 2.0   Jan 22 2010 11:37:14   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB003                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    22.01.2010                                       *
      *    STAND:    22.01.2010                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!22.01.10!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB003                           *
      ******************************************************************
        05  WB-NI-VIVBB003-TAB.
           10  WB-NI-VIVBB003-ELE  PIC S9(04) COMP   OCCURS 14  TIMES.
        05  WB-NI-VIVBB003         REDEFINES  WB-NI-VIVBB003-TAB.
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
           10 WB-NI-ED-VORLAGE     PIC S9(04) COMP.
        05  WB-VIVBB003.
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
           10 WB-ED-VORLAGE        PIC S9(09)  USAGE COMP   VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
