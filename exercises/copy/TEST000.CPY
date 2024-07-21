      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   test000.CPY  $
      *    $Revision:   1.6  $
      *
      *    $Log:   L:/MF/CPY/test000.CPv  $
      *
      *   Rev 1.6   Jan 14 2003 15:35:52   p13sp16
      *Jetzt mal eine Beschreibung, die ich hier nicht mit einem Return umbreche sondern umbrechen lasse um zu sehen ob das was ausmacht
      *
      *   Rev 1.5   Jan 14 2003 12:13:48   p13sp16
      *Das ist ein Test, ob Checkin den Quelltext verändert, was
      *er ja nicht soll....
      *
      *   Rev 2.0   Nov 26 2002 10:04:04   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB009                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.EHRMANN                                        *
      *    DATUM:    26.11.2002                                       *
      *    STAND:    26.11.2002                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00649!26.11.02!4.0 ! NEU                       ! D.EHRMANN*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB009                           *
      ******************************************************************
      * und hier ein Kommentar
	05  WB-NI-VIVBB009-TAB.
           10  WB-NI-VIVBB009-ELE  PIC S9(04)  COMP   OCCURS  30 TIMES.
        05  WB-NI-VIVBB009          REDEFINES   WB-NI-VIVBB009-TAB.
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
        05  WB-VIVBB009.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *
      ******************************************************************

