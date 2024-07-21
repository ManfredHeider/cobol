      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0113.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0113.CPv  $
      *
      *   Rev 2.1   Sep 09 2008 12:05:56   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Dialog 1)
      *
      *   Rev 2.0   Jun 26 2008 13:32:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0113                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0113 (VSWP)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    26.06.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!26.06.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0113                          *
      *****************************************************************
        05  WB-NI-VIVB0113-TAB.
           10 WB-NI-VIVB0113-ELE   PIC S9(04)  BINARY OCCURS  20 TIMES.
        05  WB-NI-VIVB0113         REDEFINES   WB-NI-VIVB0113-TAB.
           10 WB-NI-VSWP-LFDNR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-VSWP-LEISTUNG         PIC S9(04)       BINARY.
           10 WB-NI-VSWP-BIS-STAT         PIC S9(04)       BINARY.
           10 WB-NI-VSWP-ART              PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EQF1             PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EQF2             PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EQF3             PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EH-EG-NEU        PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EH-GRP           PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EH-GES           PIC S9(04)       BINARY.
           10 WB-NI-VSWP-AUFST-BED        PIC S9(04)       BINARY.
           10 WB-NI-VSWP-VERTR-2007       PIC S9(04)       BINARY.
           10 WB-NI-VSWP-BES-ZUL          PIC S9(04)       BINARY.
           10 WB-NI-VSWP-40PROZ           PIC S9(04)       BINARY.
           10 WB-NI-VSWP-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-VSWP-VBP-NR           PIC S9(04)       BINARY.
           10 WB-NI-VSWP-STATUS           PIC S9(04)       BINARY.
           10 WB-NI-VSWP-LAST-UPDATE      PIC S9(04)       BINARY.

        05  WB-VIVB0113.
           10 WB-VSWP-LFDNR               PIC S9(04)       BINARY.
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-VSWP-LEISTUNG            PIC  X(01).
           10 WB-VSWP-BIS-STAT            PIC  X(10).
           10 WB-VSWP-ART                 PIC  X(01).
           10 WB-VSWP-EQF1                PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EQF2                PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EQF3                PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EH-EG-NEU           PIC S9(09)V9(01) COMP-3.
           10 WB-VSWP-EH-GRP              PIC S9(09)V9(01) COMP-3.
           10 WB-VSWP-EH-GES              PIC S9(09)V9(01) COMP-3.
           10 WB-VSWP-AUFST-BED           PIC S9(04)       BINARY.
           10 WB-VSWP-VERTR-2007          PIC  X(01).
           10 WB-VSWP-BES-ZUL             PIC  X(01).
           10 WB-VSWP-40PROZ              PIC  X(01).
           10 WB-VSWP-MS-NR               PIC S9(04)       BINARY.
           10 WB-VSWP-VBP-NR              PIC S9(04)       BINARY.
           10 WB-VSWP-STATUS              PIC  X(01).
           10 WB-VSWP-LAST-UPDATE         PIC  X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
