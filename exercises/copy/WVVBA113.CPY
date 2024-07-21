      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA113.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA113.CPv  $
      *
      *   Rev 2.1   Aug 31 2009 09:33:42   t13ex43
      *ASL00335: PGM - VSW Release 1.1 (Dialog)
      *
      *   Rev 2.0   Aug 05 2009 09:57:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA113                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA113 (VSWP)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    05.08.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00335!05.08.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA113                           *
      *****************************************************************

        05 WB-NI-VIVBA113-TAB.
           10 WB-NI-VIVBA113-ELE   PIC S9(04)  BINARY OCCURS  23.
        05 WB-NI-VIVBA113          REDEFINES   WB-NI-VIVBA113-TAB.
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
           10 WB-NI-VSWP-EH-EG-NEU-P      PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EH-GRP-P         PIC S9(04)       BINARY.
           10 WB-NI-VSWP-EH-GES-P         PIC S9(04)       BINARY.

        05 WB-VIVBA113.
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
           10 WB-VSWP-EH-EG-NEU-P         PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EH-GRP-P            PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EH-GES-P            PIC S9(03)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *
      *****************************************************************
