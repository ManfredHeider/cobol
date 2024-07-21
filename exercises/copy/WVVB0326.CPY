      *****************************************************************
      *    COPY-MEMBER: WVVB0326                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0326 (SWBAS   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    23.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580|23.09.19|2.1 | NEU                       | TTH       *
      *         |        |    |                           |           *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0326                          *
      *****************************************************************

        05 WB-NI-VIVB0326-TAB.
           10 WB-NI-VIVB0326-ELE   PIC S9(04)  BINARY OCCURS  21.
        05 WB-NI-VIVB0326          REDEFINES   WB-NI-VIVB0326-TAB.
           10 WB-NI-SWBAS-BETR-NR         PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-UNTST-NR        PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-DAT-WECHSEL     PIC S9(04)       BINARY.
           10 WB-NI-KTY-UST-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-UST-TYP           PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-KORR-AB         PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-KORR-BIS        PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-KORR-ENDE       PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-BASIS-AB        PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-BASIS-BIS       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ZUSTRUK-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ZUSTRUK-TYP       PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-DIR-NEU         PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-EBENE           PIC S9(04)       BINARY.
           10 WB-NI-KTY-KORR-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KNR-KORR-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KNR-KORR-DAT          PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-EH-BASIS-12     PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-ABR-MONATE      PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-EH-MONWERT      PIC S9(04)       BINARY.
           10 WB-NI-SWBAS-LAST-UPDATE     PIC S9(04)       BINARY.

        05 WB-VIVB0326.
           10 WB-SWBAS-BETR-NR            PIC S9(09)       BINARY.
           10 WB-SWBAS-UNTST-NR           PIC S9(09)       BINARY.
           10 WB-SWBAS-DAT-WECHSEL        PIC  X(10).
           10 WB-KTY-UST-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-UST-TYP              PIC S9(04)       BINARY.
           10 WB-SWBAS-KORR-AB            PIC  X(10).
           10 WB-SWBAS-KORR-BIS           PIC  X(10).
           10 WB-SWBAS-KORR-ENDE          PIC  X(10).
           10 WB-SWBAS-BASIS-AB           PIC  X(10).
           10 WB-SWBAS-BASIS-BIS          PIC  X(10).
           10 WB-KTY-ZUSTRUK-TYP          PIC S9(04)       BINARY.
           10 WB-KNR-ZUSTRUK-TYP          PIC S9(04)       BINARY.
           10 WB-SWBAS-DIR-NEU            PIC S9(04)       BINARY.
           10 WB-SWBAS-EBENE              PIC S9(04)       BINARY.
           10 WB-KTY-KORR-TYP             PIC S9(04)       BINARY.
           10 WB-KNR-KORR-TYP             PIC S9(04)       BINARY.
           10 WB-KNR-KORR-DAT             PIC  X(10).
           10 WB-SWBAS-EH-BASIS-12        PIC S9(09)V9(01) COMP-3.
           10 WB-SWBAS-ABR-MONATE         PIC S9(04)       BINARY.
           10 WB-SWBAS-EH-MONWERT         PIC S9(09)V9(01) COMP-3.
           10 WB-SWBAS-LAST-UPDATE        PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  21                    *
      *****************************************************************







