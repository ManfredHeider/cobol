      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBD009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBD009.CPv  $
      *
      *   Rev 2.1   Jun 03 2015 12:36:46   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - Tabelle MS erweit
      *ern
      *
      *   Rev 2.0   Feb 10 2015 09:48:54   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBD009                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    11.02.2015                                       *
      *    STAND:    11.02.2015                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!11.02.15!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBD009                           *
      ******************************************************************
        05  WB-NI-VIVBD009-TAB.
           10  WB-NI-VIVBD009-ELE  PIC S9(04)  COMP   OCCURS  37 TIMES.
        05  WB-NI-VIVBD009          REDEFINES   WB-NI-VIVBD009-TAB.
           10 WB-NI-MS-NR               PIC S9(04)  COMP.
           10 WB-NI-MS-EINSTIEG         PIC S9(04)  COMP.
           10 WB-NI-MS-HPT              PIC S9(04)  COMP.
           10 WB-NI-MS-34C              PIC S9(04)  COMP.
           10 WB-NI-MS-ABZPROZ-REGEL    PIC S9(04)  COMP.
           10 WB-NI-MS-EING-EH-EIG      PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EIG-GES       PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EIG-PZTR      PIC S9(04)  COMP.
           10 WB-NI-MS-EH-GRU-PZTR      PIC S9(04)  COMP.
           10 WB-NI-MS-EH-GES-PZTR      PIC S9(04)  COMP.
           10 WB-NI-MS-WEIT-EH-EIG      PIC S9(04)  COMP.
           10 WB-NI-MS-ZEITRAUM         PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-EIG          PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-GRU          PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-GES          PIC S9(04)  COMP.
           10 WB-NI-MS-PROV-STF         PIC S9(04)  COMP.
           10 WB-NI-MS-GUELTIG-BIS      PIC S9(04)  COMP.
           10 WB-NI-MS-BEZ-KURZ         PIC S9(04)  COMP.
           10 WB-NI-MS-BEZ              PIC S9(04)  COMP.
           10 WB-NI-MS-FG               PIC S9(04)  COMP.
           10 WB-NI-MS-127              PIC S9(04)  COMP.
           10 WB-NI-KTY-GESELL-TYP      PIC S9(04)  COMP.
           10 WB-NI-KNR-GESELL-TYP      PIC S9(04)  COMP.
           10 WB-NI-KTY-BEFBED-TYP      PIC S9(04)  COMP.
           10 WB-NI-KNR-BEFBED-TYP      PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EG-MAX-PZTR   PIC S9(04)  COMP.
           10 WB-NI-MS-ANZ-PARTNER      PIC S9(04)  COMP.
           10 WB-NI-MS-HAUPTB-ANZPARTN  PIC S9(04)  COMP.
           10 WB-NI-MS-HAUPTB-EPARTN    PIC S9(04)  COMP.
           10 WB-NI-MS-HAUPTB-ZTRPARTN  PIC S9(04)  COMP.
           10 WB-NI-MS-HB-ANZP-ERSATZ   PIC S9(04)  COMP.
           10 WB-NI-MS-HB-EINHP-ERSATZ  PIC S9(04)  COMP.
           10 WB-NI-MS-HB-MAXP-ERSATZ   PIC S9(04)  COMP.
           10 WB-NI-MS-ANZPART-AB-RGS   PIC S9(04)  COMP.
           10 WB-NI-MS-EH-NEU-GES-PZ    PIC S9(04)  COMP.
           10 WB-NI-MS-EH-NEU-EG-NP-PZ  PIC S9(04)  COMP.
           10 WB-NI-MS-ABZPROZ-REG-NP   PIC S9(04)  COMP.
        05  WB-VIVBD009.
           10 WB-MS-NR                 PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-EINSTIEG           PIC X(1)             VALUE SPACE.
           10 WB-MS-HPT                PIC X(1)             VALUE SPACE.
           10 WB-MS-34C                PIC X(1)             VALUE SPACE.
           10 WB-MS-ABZPROZ-REGEL      PIC S999V99 COMP-3   VALUE ZERO.
           10 WB-MS-EING-EH-EIG        PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-EH-EIG-GES         PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-EH-EIG-PZTR        PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-EH-GRU-PZTR        PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-EH-GES-PZTR        PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-WEIT-EH-EIG        PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-ZEITRAUM           PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-EQF-EIG            PIC S999V99 COMP-3   VALUE ZERO.
           10 WB-MS-EQF-GRU            PIC S999V99 COMP-3   VALUE ZERO.
           10 WB-MS-EQF-GES            PIC S999V99 COMP-3   VALUE ZERO.
           10 WB-MS-PROV-STF           PIC S999V99 COMP-3   VALUE ZERO.
           10 WB-MS-GUELTIG-BIS        PIC X(10)            VALUE SPACE.
           10 WB-MS-BEZ-KURZ           PIC X(4)             VALUE SPACE.
           10 WB-MS-BEZ                PIC X(40)            VALUE SPACE.
           10 WB-MS-FG                 PIC X(1)             VALUE SPACE.
           10 WB-MS-127                PIC X(1)             VALUE SPACE.
           10 WB-KTY-GESELL-TYP        PIC S9(4) COMP       VALUE ZERO.
           10 WB-KNR-GESELL-TYP        PIC S9(4) COMP       VALUE ZERO.
           10 WB-KTY-BEFBED-TYP        PIC S9(4) COMP       VALUE ZERO.
           10 WB-KNR-BEFBED-TYP        PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-EH-EG-MAX-PZTR     PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-ANZ-PARTNER        PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-HAUPTB-ANZPARTN    PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-HAUPTB-EPARTN      PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-HAUPTB-ZTRPARTN    PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-HB-ANZP-ERSATZ     PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-HB-EINHP-ERSATZ    PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-HB-MAXP-ERSATZ     PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-ANZPART-AB-RGS     PIC S9(4) COMP       VALUE ZERO.
           10 WB-MS-EH-NEU-GES-PZ      PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-EH-NEU-EG-NP-PZ    PIC S9(09)V9 COMP-3  VALUE ZERO.
           10 WB-MS-ABZPROZ-REG-NP     PIC S999V99 COMP-3   VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 37      *
      ******************************************************************

