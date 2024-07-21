      *****************************************************************
      *    COPY-MEMBER: WVVB0205                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0205 (QEDD    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    25.03.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01103!25.03.11!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0205                           *
      *****************************************************************

        05 WB-NI-VIVB0205-TAB.
           10 WB-NI-VIVB0205-ELE   PIC S9(04)  BINARY OCCURS  16.
        05 WB-NI-VIVB0205          REDEFINES   WB-NI-VIVB0205-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-ED-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-QEDD-ED-ZUSATZ        PIC S9(04)       BINARY.
           10 WB-NI-ED-GENERATION         PIC S9(04)       BINARY.
           10 WB-NI-QEDD-DATUM            PIC S9(04)       BINARY.
           10 WB-NI-QEDD-ED-EINH          PIC S9(04)       BINARY.
           10 WB-NI-QEDD-EINH-MAX         PIC S9(04)       BINARY.
           10 WB-NI-QEDD-EINH-GEW         PIC S9(04)       BINARY.
           10 WB-NI-QEDD-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-DL-NET-GES-EH         PIC S9(04)       BINARY.
           10 WB-NI-DL-EH-MAX-PROZ        PIC S9(04)       BINARY.
           10 WB-NI-ED-EINH-FAKTOR        PIC S9(04)       BINARY.
           10 WB-NI-QUARTAL               PIC S9(04)       BINARY.
           10 WB-NI-QEDD-LAST-UPDATE      PIC S9(04)       BINARY.

        05 WB-VIVB0205.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-ED-ST-NR                 PIC S9(09)       BINARY.
           10 WB-QEDD-ED-ZUSATZ           PIC S9(04)       BINARY.
           10 WB-ED-GENERATION            PIC S9(04)       BINARY.
           10 WB-QEDD-DATUM               PIC  X(10).
           10 WB-QEDD-ED-EINH             PIC S9(09)V9(01) COMP-3.
           10 WB-QEDD-EINH-MAX            PIC S9(09)V9(01) COMP-3.
           10 WB-QEDD-EINH-GEW            PIC S9(09)V9(01) COMP-3.
           10 WB-QEDD-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-DL-NET-GES-EH            PIC S9(09)V9(01) COMP-3.
           10 WB-DL-EH-MAX-PROZ           PIC S9(03)V9(03) COMP-3.
           10 WB-ED-EINH-FAKTOR           PIC S9(03)V9(03) COMP-3.
           10 WB-QUARTAL                  PIC  X(17).
           10 WB-QEDD-LAST-UPDATE         PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************
