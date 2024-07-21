      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0300.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0300.CPv  $
      *
      *   Rev 2.1   Mar 25 2009 09:31:28   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Mar 24 2009 14:45:00   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0300                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0300 (CPROF)                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    24.03.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!24.03.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0300                          *
      *****************************************************************
        05  WB-NI-VIVB0300-TAB.
           10 WB-NI-VIVB0300-ELE   PIC S9(04)  BINARY OCCURS 28 TIMES.
        05  WB-NI-VIVB0300         REDEFINES   WB-NI-VIVB0300-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-CPROF-MONAT           PIC S9(04)       BINARY.
           10 WB-NI-KTY-DISZIPLIN         PIC S9(04)       BINARY.
           10 WB-NI-KNR-DISZIPLIN         PIC S9(04)       BINARY.
           10 WB-NI-CPROF-STERN           PIC S9(04)       BINARY.
           10 WB-NI-CPROF-ART             PIC S9(04)       BINARY.
           10 WB-NI-CPROF-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-CPROF-U-EH-GES        PIC S9(04)       BINARY.
           10 WB-NI-CPROF-U-EH-EGFR       PIC S9(04)       BINARY.
           10 WB-NI-CPROF-U-EH-WERTG      PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EQF-GES         PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EQF-EGFR        PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EQF-WERTG       PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EGFR-AB         PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EGFR-BIS        PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-EH1           PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-EH2           PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-EH3           PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-EH4           PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-STEIG1        PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-STEIG2        PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-STEIG3        PIC S9(04)       BINARY.
           10 WB-NI-CPROF-W-STEIGGES      PIC S9(04)       BINARY.
           10 WB-NI-CPROF-ANZ-ASTRUK      PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EH-NP           PIC S9(04)       BINARY.
           10 WB-NI-CPROF-EH-GES          PIC S9(04)       BINARY.
           10 WB-NI-CPROF-ANTEIL-NP       PIC S9(04)       BINARY.
           10 WB-NI-CPROF-ED-ANZ          PIC S9(04)       BINARY.

        05  WB-VIVB0300.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-CPROF-MONAT              PIC  X(10).
           10 WB-KTY-DISZIPLIN            PIC S9(04)       BINARY.
           10 WB-KNR-DISZIPLIN            PIC S9(04)       BINARY.
           10 WB-CPROF-STERN              PIC S9(04)       BINARY.
           10 WB-CPROF-ART                PIC  X(01).
           10 WB-CPROF-LAST-UPDATE        PIC  X(26).
           10 WB-CPROF-U-EH-GES           PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-U-EH-EGFR          PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-U-EH-WERTG         PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-EQF-GES            PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-EQF-EGFR           PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-EQF-WERTG          PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-EGFR-AB            PIC  X(10).
           10 WB-CPROF-EGFR-BIS           PIC  X(10).
           10 WB-CPROF-W-EH1              PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-W-EH2              PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-W-EH3              PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-W-EH4              PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-W-STEIG1           PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-W-STEIG2           PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-W-STEIG3           PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-W-STEIGGES         PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-ANZ-ASTRUK         PIC S9(04)       BINARY.
           10 WB-CPROF-EH-NP              PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-EH-GES             PIC S9(09)V9(01) COMP-3.
           10 WB-CPROF-ANTEIL-NP          PIC S9(03)V9(02) COMP-3.
           10 WB-CPROF-ED-ANZ             PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 28      *
      ******************************************************************
