      *****************************************************************
      *    COPY-MEMBER: WVVBA005                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA005 (RFT     )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.10.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01125!15.10.10!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA005                           *
      *****************************************************************

        05 WB-NI-VIVBA005-TAB.
           10 WB-NI-VIVBA005-ELE   PIC S9(04)  BINARY OCCURS  13.
        05 WB-NI-VIVBA005          REDEFINES   WB-NI-VIVBA005-TAB.
           10 WB-NI-RFT-ST-NR             PIC S9(04)       BINARY.
           10 WB-NI-RFT-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-RFT-PROV-STZ-AP       PIC S9(04)       BINARY.
           10 WB-NI-RFT-PROV-STZ-GP       PIC S9(04)       BINARY.
           10 WB-NI-RFT-PROZ-STZ          PIC S9(04)       BINARY.
           10 WB-NI-RFT-RCK-STZ           PIC S9(04)       BINARY.
           10 WB-NI-RFT-RCK-STZ-DAT       PIC S9(04)       BINARY.
           10 WB-NI-RFT-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-RFT-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-RFT-ABLAUF-DAT        PIC S9(04)       BINARY.
           10 WB-NI-KTY-RF-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-RF-TYP            PIC S9(04)       BINARY.

        05 WB-VIVBA005.
           10 WB-RFT-ST-NR                PIC S9(09)       BINARY.
           10 WB-RFT-LAST-UPDATE          PIC  X(26).
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-RFT-PROV-STZ-AP          PIC S9(03)V9(02) COMP-3.
           10 WB-RFT-PROV-STZ-GP          PIC S9(03)V9(02) COMP-3.
           10 WB-RFT-PROZ-STZ             PIC S9(03)V9(02) COMP-3.
           10 WB-RFT-RCK-STZ              PIC S9(03)V9(02) COMP-3.
           10 WB-RFT-RCK-STZ-DAT          PIC  X(10).
           10 WB-RFT-DAT-VON              PIC  X(10).
           10 WB-RFT-DAT-BIS              PIC  X(10).
           10 WB-RFT-ABLAUF-DAT           PIC  X(10).
           10 WB-KTY-RF-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-RF-TYP               PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************
