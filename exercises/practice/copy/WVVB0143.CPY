      *****************************************************************
      *    COPY-MEMBER: WVVB0143                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0143 (BWKS    )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THI / TTH                                        *
      *    DATUM:    22.06.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-1742 !22.06.21! NEU                           ! THI / TTH *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0143                          *
      *****************************************************************

        05 WB-NI-VIVB0143-TAB.
           10 WB-NI-VIVB0143-ELE   PIC S9(04)  BINARY OCCURS  16.
        05 WB-NI-VIVB0143          REDEFINES   WB-NI-VIVB0143-TAB.
           10 WB-NI-BWKS-BETR-NR          PIC S9(04)       BINARY.
           10 WB-NI-BWKS-UNTST-NR         PIC S9(04)       BINARY.
           10 WB-NI-BWKS-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-BWKS-DAT-BIS          PIC S9(04)       BINARY.
           10 WB-NI-BWKS-DAT-ERSTELL      PIC S9(04)       BINARY.
           10 WB-NI-BWKS-DAT-ENDE         PIC S9(04)       BINARY.
           10 WB-NI-BWKS-HERKUNFT         PIC S9(04)       BINARY.
           10 WB-NI-BWKS-EBENE            PIC S9(04)       BINARY.
           10 WB-NI-BWKS-ABZUGS-ART       PIC S9(04)       BINARY.
           10 WB-NI-BWKS-DIR-NEU          PIC S9(04)       BINARY.
           10 WB-NI-BWKS-DIR-ALT          PIC S9(04)       BINARY.
           10 WB-NI-BWKS-BETR-ALT         PIC S9(04)       BINARY.
           10 WB-NI-BWKS-ZUSTRUK-TYP      PIC S9(04)       BINARY.
           10 WB-NI-BWKS-STAT             PIC S9(04)       BINARY.
           10 WB-NI-BWKS-STAT-DATUM       PIC S9(04)       BINARY.
           10 WB-NI-BWKS-LAST-UPDATE      PIC S9(04)       BINARY.

        05 WB-VIVB0143.
           10 WB-BWKS-BETR-NR             PIC S9(09)       BINARY.
           10 WB-BWKS-UNTST-NR            PIC S9(09)       BINARY.
           10 WB-BWKS-DAT-VON             PIC  X(10).
           10 WB-BWKS-DAT-BIS             PIC  X(10).
           10 WB-BWKS-DAT-ERSTELL         PIC  X(10).
           10 WB-BWKS-DAT-ENDE            PIC  X(10).
           10 WB-BWKS-HERKUNFT            PIC  X(01).
           10 WB-BWKS-EBENE               PIC S9(04)       BINARY.
           10 WB-BWKS-ABZUGS-ART          PIC S9(04)       BINARY.
           10 WB-BWKS-DIR-NEU             PIC S9(04)       BINARY.
           10 WB-BWKS-DIR-ALT             PIC S9(04)       BINARY.
           10 WB-BWKS-BETR-ALT            PIC S9(09)       BINARY.
           10 WB-BWKS-ZUSTRUK-TYP         PIC S9(04)       BINARY.
           10 WB-BWKS-STAT                PIC S9(04)       BINARY.
           10 WB-BWKS-STAT-DATUM          PIC  X(10).
           10 WB-BWKS-LAST-UPDATE         PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
