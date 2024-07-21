      *****************************************************************
      *    COPY-MEMBER: WVVB0314                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0314 (PARTDW  )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    08.03.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS01008!08.03.12!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0314                           *
      *****************************************************************

        05 WB-NI-VIVB0314-TAB.
           10 WB-NI-VIVB0314-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0314          REDEFINES   WB-NI-VIVB0314-TAB.
           10 WB-NI-PARTDW-UNTST          PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-BETR           PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-EBENE          PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-MONAT          PIC S9(04)       BINARY.
           10 WB-NI-KTY-ZUSTRUK-TYP       PIC S9(04)       BINARY.
           10 WB-NI-KNR-ZUSTRUK-TYP       PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-EH-NE          PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-EH-NNE         PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-USTRU-DAT      PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-LAST-UPD       PIC S9(04)       BINARY.
           10 WB-NI-PARTDW-BEMERKUNG      PIC S9(04)       BINARY.

        05 WB-VIVB0314.
           10 WB-PARTDW-UNTST             PIC S9(09)       BINARY.
           10 WB-PARTDW-BETR              PIC S9(09)       BINARY.
           10 WB-PARTDW-EBENE             PIC S9(04)       BINARY.
           10 WB-PARTDW-MONAT             PIC  X(10).
           10 WB-KTY-ZUSTRUK-TYP          PIC S9(04)       BINARY.
           10 WB-KNR-ZUSTRUK-TYP          PIC S9(04)       BINARY.
           10 WB-PARTDW-EH-NE             PIC S9(09)V9(01) COMP-3.
           10 WB-PARTDW-EH-NNE            PIC S9(09)V9(01) COMP-3.
           10 WB-PARTDW-USTRU-DAT         PIC  X(10).
           10 WB-PARTDW-LAST-UPD          PIC  X(26).
           10 WB-PARTDW-BEMERKUNG         PIC  X(20).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
