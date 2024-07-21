      *****************************************************************
      *    COPY-MEMBER: WVVB0114                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0114 (VSWE)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    16.07.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!16.07.08!2.1 ! NEU                       ! KLEEMEIER *
080722* VLA-5045!08.07.22!2.2 ! Num. gepackte Felder      ! HEIM     
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0114                          *
      *****************************************************************
        05  WB-NI-VIVB0114-TAB.
           10 WB-NI-VIVB0114-ELE   PIC S9(04)  BINARY OCCURS  32 TIMES.
        05  WB-NI-VIVB0114         REDEFINES   WB-NI-VIVB0114-TAB.
           10 WB-NI-VSWE-LFDNR            PIC S9(04)       BINARY.
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.
           10 WB-NI-VSWE-LEISTUNG         PIC S9(04)       BINARY.
           10 WB-NI-VSWE-ART              PIC S9(04)       BINARY.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-VSWE-ERF-KZ           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-VSWE-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-VSWE-VBP-NR           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-EG-NEU        PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-EG-KORR       PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-EG-GES        PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-GRP           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-GRP-KORR      PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-GES           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-EH-GES-KORR      PIC S9(04)       BINARY.
           10 WB-NI-VSWE-JEQF-EG          PIC S9(04)       BINARY.
           10 WB-NI-VSWE-JEQF-GRP         PIC S9(04)       BINARY.
           10 WB-NI-VSWE-JEQF-GES         PIC S9(04)       BINARY.
           10 WB-NI-MAT-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-BES              PIC S9(04)       BINARY.
           10 WB-NI-VSWE-NEU-KZ           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-VERTR-KZ         PIC S9(04)       BINARY.
           10 WB-NI-VSWE-BES-ZUL-KZ       PIC S9(04)       BINARY.
           10 WB-NI-VSWE-ALTERSGR-KZ      PIC S9(04)       BINARY.
           10 WB-NI-VSWE-VERS-NR-LV       PIC S9(04)       BINARY.
           10 WB-NI-VSWE-VERSB-LV         PIC S9(04)       BINARY.
           10 WB-NI-VSWE-VERS-NR-RV       PIC S9(04)       BINARY.
           10 WB-NI-VSWE-VERSB-RV         PIC S9(04)       BINARY.
           10 WB-NI-VSWE-ANZ-ED           PIC S9(04)       BINARY.
           10 WB-NI-VSWE-AV-BETRAG        PIC S9(04)       BINARY.

        05  WB-VIVB0114.
           10 WB-VSWE-LFDNR               PIC S9(04)       BINARY.
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 WB-VSWE-LEISTUNG            PIC  X(01).
           10 WB-VSWE-ART                 PIC  X(01).
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-VSWE-ERF-KZ              PIC  X(02).
           10 WB-VSWE-LAST-UPDATE         PIC  X(26).
           10 WB-VSWE-MS-NR               PIC S9(04)       BINARY.
           10 WB-VSWE-VBP-NR              PIC S9(04)       BINARY.
080722     10 WB-VSWE-EH-EG-NEU           PIC S9(10)V9(01) COMP-3.
080722     10 WB-VSWE-EH-EG-KORR          PIC S9(10)V9(01) COMP-3.
080722     10 WB-VSWE-EH-EG-GES           PIC S9(10)V9(01) COMP-3.
080722     10 WB-VSWE-EH-GRP              PIC S9(10)V9(01) COMP-3.
080722     10 WB-VSWE-EH-GRP-KORR         PIC S9(10)V9(01) COMP-3.
080722     10 WB-VSWE-EH-GES              PIC S9(10)V9(01) COMP-3.
080722     10 WB-VSWE-EH-GES-KORR         PIC S9(10)V9(01) COMP-3.
           10 WB-VSWE-JEQF-EG             PIC S9(03)V9(02) COMP-3.
           10 WB-VSWE-JEQF-GRP            PIC S9(03)V9(02) COMP-3.
           10 WB-VSWE-JEQF-GES            PIC S9(03)V9(02) COMP-3.
           10 WB-MAT-DAT-BIS              PIC  X(10).
           10 WB-VSWE-BES                 PIC S9(04)       BINARY.
           10 WB-VSWE-NEU-KZ              PIC  X(03).
           10 WB-VSWE-VERTR-KZ            PIC  X(01).
           10 WB-VSWE-BES-ZUL-KZ          PIC  X(01).
           10 WB-VSWE-ALTERSGR-KZ         PIC  X(01).
           10 WB-VSWE-VERS-NR-LV          PIC S9(15)       COMP-3.
           10 WB-VSWE-VERSB-LV            PIC S9(09)V9(02) COMP-3.
           10 WB-VSWE-VERS-NR-RV          PIC S9(15)       COMP-3.
           10 WB-VSWE-VERSB-RV            PIC S9(09)V9(02) COMP-3.
           10 WB-VSWE-ANZ-ED              PIC S9(04)       BINARY.
           10 WB-VSWE-AV-BETRAG           PIC S9(09)V9(02) COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
