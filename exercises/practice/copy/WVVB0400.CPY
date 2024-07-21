      *****************************************************************
      *    COPY-MEMBER: WVVB0400                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0400 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    12.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!12.05.16!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0400                           *
      *****************************************************************
 
        05 WB-NI-VIVB0400-TAB.
           10 WB-NI-VIVB0400-ELE   PIC S9(04)  BINARY OCCURS  31.
        05 WB-NI-VIVB0400          REDEFINES   WB-NI-VIVB0400-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-STAB-BUCH-DAT         PIC S9(04)       BINARY.
           10 WB-NI-STAB-BEZUGS-JAHR      PIC S9(04)       BINARY.
           10 WB-NI-STAB-STEUER-JAHR      PIC S9(04)       BINARY.
           10 WB-NI-STAB-DAT-HERKUNFT     PIC S9(04)       BINARY.
           10 WB-NI-STAB-LFD              PIC S9(04)       BINARY.
           10 WB-NI-STAB-BEZUG-LFD        PIC S9(04)       BINARY.
           10 WB-NI-STAB-ABR-NR           PIC S9(04)       BINARY.
           10 WB-NI-KTY-VERARB-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-VERARB-TYP        PIC S9(04)       BINARY.
           10 WB-NI-STAB-SHKZ             PIC S9(04)       BINARY.
           10 WB-NI-STAB-BUCH-TEXT        PIC S9(04)       BINARY.
           10 WB-NI-STAB-BUCH-TEXT2       PIC S9(04)       BINARY.
           10 WB-NI-KTY-USTAT-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KNR-USTAT-TYP         PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-STAB-KONTO-TYP        PIC S9(04)       BINARY.
           10 WB-NI-STAB-BELEGART         PIC S9(04)       BINARY.
           10 WB-NI-STAB-BELEG-NR         PIC S9(04)       BINARY.
           10 WB-NI-STAB-TURNUS           PIC S9(04)       BINARY.
           10 WB-NI-STAB-BELEG-DAT        PIC S9(04)       BINARY.
           10 WB-NI-STAB-KOSTEN-STELLE    PIC S9(04)       BINARY.
           10 WB-NI-STAB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-STAB-KONTO-KU         PIC S9(04)       BINARY.
           10 WB-NI-STAB-BETRAG-KU        PIC S9(04)       BINARY.
           10 WB-NI-STAB-KONTO-U          PIC S9(04)       BINARY.
           10 WB-NI-STAB-BETRAG-U         PIC S9(04)       BINARY.
           10 WB-NI-STAB-UST-ANTEIL-U     PIC S9(04)       BINARY.
           10 WB-NI-STAB-STS              PIC S9(04)       BINARY.
           10 WB-NI-STAB-UST-BERECHNET    PIC S9(04)       BINARY.
 
        05 WB-VIVB0400.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-STAB-BUCH-DAT            PIC  X(10).
           10 WB-STAB-BEZUGS-JAHR         PIC S9(04)       BINARY.
           10 WB-STAB-STEUER-JAHR         PIC S9(04)       BINARY.
           10 WB-STAB-DAT-HERKUNFT        PIC  X(01).
           10 WB-STAB-LFD                 PIC S9(09)       BINARY.
           10 WB-STAB-BEZUG-LFD           PIC S9(09)       BINARY.
           10 WB-STAB-ABR-NR              PIC  X(15).
           10 WB-KTY-VERARB-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-VERARB-TYP           PIC S9(04)       BINARY.
           10 WB-STAB-SHKZ                PIC  X(01).
           10 WB-STAB-BUCH-TEXT           PIC  X(40).
           10 WB-STAB-BUCH-TEXT2          PIC  X(52).
           10 WB-KTY-USTAT-TYP            PIC S9(04)       BINARY.
           10 WB-KNR-USTAT-TYP            PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-STAB-KONTO-TYP           PIC  X(01).
           10 WB-STAB-BELEGART            PIC  X(03).
           10 WB-STAB-BELEG-NR            PIC  X(07).
           10 WB-STAB-TURNUS              PIC  X(06).
           10 WB-STAB-BELEG-DAT           PIC  X(08).
           10 WB-STAB-KOSTEN-STELLE       PIC  X(08).
           10 WB-STAB-LAST-UPDATE         PIC  X(26).
           10 WB-STAB-KONTO-KU            PIC  X(12).
           10 WB-STAB-BETRAG-KU           PIC S9(09)V9(02) COMP-3.
           10 WB-STAB-KONTO-U             PIC  X(12).
           10 WB-STAB-BETRAG-U            PIC S9(09)V9(02) COMP-3.
           10 WB-STAB-UST-ANTEIL-U        PIC S9(09)V9(02) COMP-3.
           10 WB-STAB-STS                 PIC S9(04)       BINARY.
           10 WB-STAB-UST-BERECHNET       PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

