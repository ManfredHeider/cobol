      *****************************************************************
      *    COPY-MEMBER: WVVBB113                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB113 (VSWP)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    23.10.2013                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00498!23.10.13!2.1 ! NEU                       ! Walb      *
130722* VLA-5045!13.07.22!2.2 ! Num. gepackte Felder      ! HEIM      *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB113                           *
      *****************************************************************

        05 WB-NI-VIVBB113-TAB.
           10 WB-NI-VIVBB113-ELE   PIC S9(04)  BINARY OCCURS  24.
        05 WB-NI-VIVBB113          REDEFINES   WB-NI-VIVBB113-TAB.
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
           10 WB-NI-VSWP-BS               PIC S9(04)       BINARY.

        05 WB-VIVBB113.
           10 WB-VSWP-LFDNR               PIC S9(04)       BINARY.
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-VSWP-LEISTUNG            PIC  X(01).
           10 WB-VSWP-BIS-STAT            PIC  X(10).
           10 WB-VSWP-ART                 PIC  X(01).
           10 WB-VSWP-EQF1                PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EQF2                PIC S9(03)V9(02) COMP-3.
           10 WB-VSWP-EQF3                PIC S9(03)V9(02) COMP-3.
130722     10 WB-VSWP-EH-EG-NEU           PIC S9(10)V9(01) COMP-3.
130722     10 WB-VSWP-EH-GRP              PIC S9(10)V9(01) COMP-3.
130722     10 WB-VSWP-EH-GES              PIC S9(10)V9(01) COMP-3.
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
           10 WB-VSWP-BS                  PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  24                    *
      *****************************************************************
