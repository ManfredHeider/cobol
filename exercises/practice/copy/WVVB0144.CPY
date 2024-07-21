      *****************************************************************
      *    COPY-MEMBER: WVVB0144                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0144 (BWUEB   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THI / TTH                                        *
      *    DATUM:    22.06.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-1742 !22.06.21! NEU                           ! THI / TTH *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0144                          *
      *****************************************************************

        05 WB-NI-VIVB0144-TAB.
           10 WB-NI-VIVB0144-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0144          REDEFINES   WB-NI-VIVB0144-TAB.
           10 WB-NI-BWUEB-BETR-ALT        PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-UEBH-NR         PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-DAT-BIS         PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-DAT-ERSTELL     PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-DAT-ENDE        PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-HERKUNFT        PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-BETR-NEU        PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-STAT            PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-STAT-DATUM      PIC S9(04)       BINARY.
           10 WB-NI-BWUEB-LAST-UPDATE     PIC S9(04)       BINARY.

        05 WB-VIVB0144.
           10 WB-BWUEB-BETR-ALT           PIC S9(09)       BINARY.
           10 WB-BWUEB-UEBH-NR            PIC S9(09)       BINARY.
           10 WB-BWUEB-DAT-VON            PIC  X(10).
           10 WB-BWUEB-DAT-BIS            PIC  X(10).
           10 WB-BWUEB-DAT-ERSTELL        PIC  X(10).
           10 WB-BWUEB-DAT-ENDE           PIC  X(10).
           10 WB-BWUEB-HERKUNFT           PIC  X(01).
           10 WB-BWUEB-BETR-NEU           PIC S9(09)       BINARY.
           10 WB-BWUEB-STAT               PIC S9(04)       BINARY.
           10 WB-BWUEB-STAT-DATUM         PIC  X(10).
           10 WB-BWUEB-LAST-UPDATE        PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
