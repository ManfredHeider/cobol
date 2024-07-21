      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WVVBA009.CPv  $
      *
      *   Rev 2.1   Jun 20 2002 14:25:18   t13ae13
      *AVB00606: Einführung Seminare für DVAG-Wien
      *
      *   Rev 2.0   Jun 11 2002 11:49:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************         
      *    COPY-MEMBER: WVVBA009                                      *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: S. Walb                                          *         
      *    DATUM:    13.06.2002                                       *         
      *    STAND:    13.06.2002                                       *         
      *    --------------------------------------------------------   *         
      *    VERSION:  001                                              *         
      *****************************************************************         
      *                                                                         
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00606!13.06.02!2.1 ! Neu                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA009                           *
      ******************************************************************
        05  WB-NI-VIVBA009-TAB.
           10  WB-NI-VIVBA009-ELE  PIC S9(04)  COMP   OCCURS  23 TIMES.
        05  WB-NI-VIVBA009          REDEFINES   WB-NI-VIVBA009-TAB.
           10 WB-NI-MS-NR           PIC S9(04)  COMP.
           10 WB-NI-MS-EINSTIEG     PIC S9(04)  COMP.
           10 WB-NI-MS-HPT          PIC S9(04)  COMP.
           10 WB-NI-MS-34C          PIC S9(04)  COMP.
           10 WB-NI-MS-ABZPROZ-REGEL PIC S9(04)  COMP.
           10 WB-NI-MS-EING-EH-EIG  PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EIG-GES   PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EIG-PZTR  PIC S9(04)  COMP.
           10 WB-NI-MS-EH-GRU-PZTR  PIC S9(04)  COMP.
           10 WB-NI-MS-EH-GES-PZTR  PIC S9(04)  COMP.
           10 WB-NI-MS-WEIT-EH-EIG  PIC S9(04)  COMP.
           10 WB-NI-MS-ZEITRAUM     PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-EIG      PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-GRU      PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-GES      PIC S9(04)  COMP.
           10 WB-NI-MS-PROV-STF     PIC S9(04)  COMP.
           10 WB-NI-MS-GUELTIG-BIS  PIC S9(04)  COMP.
           10 WB-NI-MS-BEZ-KURZ     PIC S9(04)  COMP.
           10 WB-NI-MS-BEZ          PIC S9(04)  COMP.
           10 WB-NI-MS-FG           PIC S9(04)  COMP.
           10 WB-NI-MS-127          PIC S9(04)  COMP.
           10 WB-NI-KTY-GESELL-TYP  PIC S9(04)  COMP.
           10 WB-NI-KNR-GESELL-TYP  PIC S9(04)  COMP.
        05  WB-VIVBA009.
           10 WB-MS-NR             PIC S9(4) COMP           VALUE ZERO.
           10 WB-MS-EINSTIEG       PIC X(1)                 VALUE SPACE.
           10 WB-MS-HPT            PIC X(1)                 VALUE SPACE.
           10 WB-MS-34C            PIC X(1)                 VALUE SPACE.
           10 WB-MS-ABZPROZ-REGEL  PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-EING-EH-EIG    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-EIG-GES     PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-EIG-PZTR    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-GRU-PZTR    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-GES-PZTR    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-WEIT-EH-EIG    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-ZEITRAUM       PIC S9(4) COMP           VALUE ZERO.
           10 WB-MS-EQF-EIG        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-EQF-GRU        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-EQF-GES        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-PROV-STF       PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-GUELTIG-BIS    PIC X(10)                VALUE SPACE.
           10 WB-MS-BEZ-KURZ       PIC X(4)                 VALUE SPACE.
           10 WB-MS-BEZ            PIC X(40)                VALUE SPACE.
           10 WB-MS-FG             PIC X(1)                 VALUE SPACE.
           10 WB-MS-127            PIC X(1)                 VALUE SPACE.
           10 WB-KTY-GESELL-TYP    PIC S9(4) COMP           VALUE ZERO.
           10 WB-KNR-GESELL-TYP    PIC S9(4) COMP           VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
