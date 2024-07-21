      *****************************************************************
      *    COPY-MEMBER: WVVBA220                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA220 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    05.07.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00567!05.07.17!2.1 ! NEU                       ! WALB      *
030822* VLA-5182!03.08.22!2.11! Korrektur num. gepackte F.! U. Heim
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA220                           *
      *****************************************************************

        05 WB-NI-VIVBA220-TAB.
           10 WB-NI-VIVBA220-ELE   PIC S9(04)  BINARY OCCURS  24.
        05 WB-NI-VIVBA220          REDEFINES   WB-NI-VIVBA220-TAB.
           10 WB-NI-AZERF-PART-VBNR       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETR-VBNR       PIC S9(04)       BINARY.
           10 WB-NI-DIR-NR                PIC S9(04)       BINARY.
           10 WB-NI-MS-NR                 PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANSPR-SEIT      PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANSPR-BIS       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EBENE           PIC S9(04)       BINARY.
           10 WB-NI-AZERF-VERT-FAKTOR     PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETR-FAKTOR     PIC S9(04)       BINARY.
           10 WB-NI-AZERF-ANZAHL-MON      PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETRAG          PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EH-GRENZE       PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EH-EIGEN        PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EH-EIGEN-MON    PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EQF-EIGEN       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-AZERF-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EG-UFREI        PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EG-UPFL         PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EG-UFREI-RECH   PIC S9(04)       BINARY.
           10 WB-NI-AZERF-EG-UPFL-RECH    PIC S9(04)       BINARY.
           10 WB-NI-AZERF-BETRAG-UPFL     PIC S9(04)       BINARY.

        05 WB-VIVBA220.
           10 WB-AZERF-PART-VBNR          PIC S9(09)       BINARY.
           10 WB-AZERF-BETR-VBNR          PIC S9(09)       BINARY.
           10 WB-DIR-NR                   PIC S9(04)       BINARY.
           10 WB-MS-NR                    PIC S9(04)       BINARY.
           10 WB-AZERF-ANSPR-VOM          PIC  X(10).
           10 WB-AZERF-ANSPR-SEIT         PIC  X(10).
           10 WB-AZERF-ANSPR-BIS          PIC  X(10).
           10 WB-AZERF-EBENE              PIC S9(04)       BINARY.
           10 WB-AZERF-VERT-FAKTOR        PIC S9(04)       BINARY.
           10 WB-AZERF-BETR-FAKTOR        PIC S9(09)V9(02) COMP-3.
           10 WB-AZERF-ANZAHL-MON         PIC S9(04)       BINARY.
           10 WB-AZERF-BETRAG             PIC S9(09)V9(02) COMP-3.
030822     10 WB-AZERF-EH-GRENZE          PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZERF-EH-EIGEN           PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZERF-EH-EIGEN-MON       PIC S9(10)V9(01) COMP-3.
           10 WB-AZERF-EQF-EIGEN          PIC S9(03)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-AZERF-LAST-UPDATE        PIC  X(26).
030822     10 WB-AZERF-EG-UFREI           PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZERF-EG-UPFL            PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZERF-EG-UFREI-RECH      PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZERF-EG-UPFL-RECH       PIC S9(10)V9(01) COMP-3.
           10 WB-AZERF-BETRAG-UPFL        PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  24                    *
      *****************************************************************

