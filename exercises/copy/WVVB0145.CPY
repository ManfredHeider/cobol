      *****************************************************************
      *    COPY-MEMBER: WVVB0145                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0145 (BWBAS   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U130213                                          *
      *    DATUM:    08.07.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-2980 !08.07.21! NEU                           ! U130213   *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0145                          *
      *****************************************************************

          05 WB-NI-VIVB0145-TAB.
             10 WB-NI-VIVB0145-ELE   PIC S9(04)  BINARY OCCURS  12.
          05 WB-NI-VIVB0145          REDEFINES   WB-NI-VIVB0145-TAB.
             10 WB-NI-BWBAS-UNTST-NR        PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-DAT-VON         PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-MS-NR           PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-DAT-ERSTELL     PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-ZUSTRUK-TYP     PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-HERKUNFT        PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-BETR-NR         PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-EBENE           PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-DIR-NEU         PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-DIR-ALT         PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-STAT            PIC S9(04)       BINARY.
             10 WB-NI-BWBAS-LAST-UPDATE     PIC S9(04)       BINARY.

          05 WB-VIVB0145.
             10 WB-BWBAS-UNTST-NR           PIC S9(09)       BINARY.
             10 WB-BWBAS-DAT-VON            PIC  X(10).
             10 WB-BWBAS-MS-NR              PIC S9(04)       BINARY.
             10 WB-BWBAS-DAT-ERSTELL        PIC  X(10).
             10 WB-BWBAS-ZUSTRUK-TYP        PIC S9(04)       BINARY.
             10 WB-BWBAS-HERKUNFT           PIC  X(01).
             10 WB-BWBAS-BETR-NR            PIC S9(09)       BINARY.
             10 WB-BWBAS-EBENE              PIC S9(04)       BINARY.
             10 WB-BWBAS-DIR-NEU            PIC S9(04)       BINARY.
             10 WB-BWBAS-DIR-ALT            PIC S9(04)       BINARY.
             10 WB-BWBAS-STAT               PIC S9(04)       BINARY.
             10 WB-BWBAS-LAST-UPDATE        PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *
      *****************************************************************
