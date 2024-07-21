      *****************************************************************
      *    COPY-MEMBER: WVVBC046                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBC046 (KLZA    )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    26.04.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * -        !26.04.22! NEU                           ! -         *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBC046                          *
      *****************************************************************

        05 WB-NI-VIVBC046-TAB.
           10 WB-NI-VIVBC046-ELE   PIC S9(04)  BINARY OCCURS  31.
        05 WB-NI-VIVBC046          REDEFINES   WB-NI-VIVBC046-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-KLZA-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.
           10 WB-NI-KLZA-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-BEITRAG          PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KLZA-ANSPR-BETRAG     PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-CENTRAL-KZ       PIC S9(04)       BINARY.
           10 WB-NI-KLZA-EH-NNE           PIC S9(04)       BINARY.
           10 WB-NI-KLZA-JEQF             PIC S9(04)       BINARY.
           10 WB-NI-KLZA-ERF-INDX         PIC S9(04)       BINARY.
           10 WB-NI-KLZA-AS-CLUB-KZ       PIC S9(04)       BINARY.
           10 WB-NI-KLZA-BES-345-KZ       PIC S9(04)       BINARY.
           10 WB-NI-KLZA-STRUKTUR         PIC S9(04)       BINARY.
           10 WB-NI-KLZA-PRAXIS           PIC S9(04)       BINARY.
           10 WB-NI-KLZA-AUSTRITT-KZ      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-BES-229-KZ       PIC S9(04)       BINARY.
           10 WB-NI-KLZA-BV-FEHLT-KZ      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-PFAEND-KZ        PIC S9(04)       BINARY.
           10 WB-NI-KLZA-SPERRE-KZ        PIC S9(04)       BINARY.
           10 WB-NI-KLZA-SPERRE-TEXT      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-EH-NNE-SOLL      PIC S9(04)       BINARY.
           10 WB-NI-KLZA-BQF              PIC S9(04)       BINARY.
           10 WB-NI-KLZA-SACH-EG-BASIS    PIC S9(04)       BINARY.
           10 WB-NI-KLZA-SACH-EG-VERGL    PIC S9(04)       BINARY.
           10 WB-NI-KLZA-SACH-EG-STEIG    PIC S9(04)       BINARY.
           10 WB-NI-KLZA-ZUGANG           PIC S9(04)       BINARY.

        05 WB-VIVBC046.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-KLZA-ANSPR-VOM           PIC  X(10).
           10 WB-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 WB-KLZA-LAST-UPDATE         PIC  X(26).
           10 WB-KLZA-BEITRAG             PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KLZA-ANSPR-BETRAG        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 WB-KLZA-CENTRAL-KZ          PIC  X(01).
           10 WB-KLZA-EH-NNE              PIC S9(09)V9(01) COMP-3.
           10 WB-KLZA-JEQF                PIC S9(03)V9(02) COMP-3.
           10 WB-KLZA-ERF-INDX            PIC  X(01).
           10 WB-KLZA-AS-CLUB-KZ          PIC  X(01).
           10 WB-KLZA-BES-345-KZ          PIC  X(01).
           10 WB-KLZA-STRUKTUR            PIC S9(04)       BINARY.
           10 WB-KLZA-PRAXIS              PIC S9(04)       BINARY.
           10 WB-KLZA-AUSTRITT-KZ         PIC  X(01).
           10 WB-KLZA-BES-229-KZ          PIC  X(01).
           10 WB-KLZA-BV-FEHLT-KZ         PIC  X(01).
           10 WB-KLZA-PFAEND-KZ           PIC  X(01).
           10 WB-KLZA-SPERRE-KZ           PIC  X(01).
           10 WB-KLZA-SPERRE-TEXT         PIC  X(40).
           10 WB-KLZA-EH-NNE-SOLL         PIC S9(09)V9(01) COMP-3.
           10 WB-KLZA-BQF                 PIC S9(03)V9(02) COMP-3.
           10 WB-KLZA-SACH-EG-BASIS       PIC S9(09)V9(01) COMP-3.
           10 WB-KLZA-SACH-EG-VERGL       PIC S9(09)V9(01) COMP-3.
           10 WB-KLZA-SACH-EG-STEIG       PIC S9(03)V9(02) COMP-3.
           10 WB-KLZA-ZUGANG              PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************
