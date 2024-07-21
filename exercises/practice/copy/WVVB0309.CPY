      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.10.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0309.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0309.CPv  $
      *
      *   Rev 2.1   Oct 29 2010 16:07:20   t13ae36
      *AAS00927: Neue 300 Einheiten Regel für alle
      *
      *   Rev 2.0   Oct 13 2010 12:08:02   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0309                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0309 (HNP     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    13.10.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00927!13.10.10!2.1 ! NEU                       ! FRANKE    *
      *         !28.10.10!    ! REV                       ! FRANKE    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0309                           *
      *****************************************************************

        05 WB-NI-VIVB0309-TAB.
           10 WB-NI-VIVB0309-ELE   PIC S9(04)  BINARY OCCURS  19.
        05 WB-NI-VIVB0309          REDEFINES   WB-NI-VIVB0309-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-HNP-MONAT             PIC S9(04)       BINARY.
           10 WB-NI-KTY-HSTATUS           PIC S9(04)       BINARY.
           10 WB-NI-KNR-HSTATUS           PIC S9(04)       BINARY.
           10 WB-NI-HNP-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-HNP-HUERDE-BASIS      PIC S9(04)       BINARY.
           10 WB-NI-HNP-EEG               PIC S9(04)       BINARY.
           10 WB-NI-HNP-HUERDE-ABS        PIC S9(04)       BINARY.
           10 WB-NI-HNP-HUERDE-REST       PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-EIGEN-KUM      PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-EIGEN          PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-EIGEN-H        PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-EIGEN-W        PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-GRUPPE         PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-GESAMT         PIC S9(04)       BINARY.
           10 WB-NI-HNP-EH-GESAMT-W       PIC S9(04)       BINARY.
           10 WB-NI-KTY-SATZ-ART          PIC S9(04)       BINARY.
           10 WB-NI-KNR-SATZ-ART          PIC S9(04)       BINARY.
           10 WB-NI-HNP-BEMERKUNG         PIC S9(04)       BINARY.

        05 WB-VIVB0309.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-HNP-MONAT                PIC  X(10).
           10 WB-KTY-HSTATUS              PIC S9(04)       BINARY.
           10 WB-KNR-HSTATUS              PIC S9(04)       BINARY.
           10 WB-HNP-LAST-UPDATE          PIC  X(26).
           10 WB-HNP-HUERDE-BASIS         PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EEG                  PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-HUERDE-ABS           PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-HUERDE-REST          PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-EIGEN-KUM         PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-EIGEN             PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-EIGEN-H           PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-EIGEN-W           PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-GRUPPE            PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-GESAMT            PIC S9(09)V9(01) COMP-3.
           10 WB-HNP-EH-GESAMT-W          PIC S9(09)V9(01) COMP-3.
           10 WB-KTY-SATZ-ART             PIC S9(04)       BINARY.
           10 WB-KNR-SATZ-ART             PIC S9(04)       BINARY.
           10 WB-HNP-BEMERKUNG            PIC  X(20).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

