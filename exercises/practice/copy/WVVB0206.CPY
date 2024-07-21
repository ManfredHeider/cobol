      *****************************************************************
      *    COPY-MEMBER: WVVB0206                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0206 (QEDA    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    28.09.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01103!28.09.10!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0206                           *
      *****************************************************************

        05 WB-NI-VIVB0206-TAB.
           10 WB-NI-VIVB0206-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0206          REDEFINES   WB-NI-VIVB0206-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-ED-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-QEDA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-QEDA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-QEDA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.

        05 WB-VIVB0206.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-ED-ST-NR                 PIC S9(09)       BINARY.
           10 WB-QEDA-ANSPR-VOM           PIC  X(10).
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-QEDA-LAST-UPDATE         PIC  X(26).
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-QEDA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
