      *****************************************************************
      *    COPY-MEMBER: WVVBA221                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBA221 (ALIAS)                        *
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
      * COBOL DEKLARATION FÜR VIEW VIVBA221                           *
      *****************************************************************

        05 WB-NI-VIVBA221-TAB.
           10 WB-NI-VIVBA221-ELE   PIC S9(04)  BINARY OCCURS  26.
        05 WB-NI-VIVBA221          REDEFINES   WB-NI-VIVBA221-TAB.
           10 WB-NI-AZNERF-PART-VBNR      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETR-VBNR      PIC S9(04)       BINARY.
           10 WB-NI-DIR-NR                PIC S9(04)       BINARY.
           10 WB-NI-MS-NR                 PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANSPR-VOM      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANSPR-SEIT     PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANSPR-BIS      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EBENE          PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-VERT-FAKTOR    PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETR-FAKTOR    PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-ANZAHL-MON     PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETRAG         PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EH-GRENZE      PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EH-EIGEN       PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EH-EIGEN-MON   PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EQF-EIGEN      PIC S9(04)       BINARY.
           10 WB-NI-KNR-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NERF-GRUND        PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-LAST-UPDATE    PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EG-UFREI       PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EG-UPFL        PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EG-UFREI-RECH  PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-EG-UPFL-RECH   PIC S9(04)       BINARY.
           10 WB-NI-AZNERF-BETRAG-UPFL    PIC S9(04)       BINARY.

        05 WB-VIVBA221.
           10 WB-AZNERF-PART-VBNR         PIC S9(09)       BINARY.
           10 WB-AZNERF-BETR-VBNR         PIC S9(09)       BINARY.
           10 WB-DIR-NR                   PIC S9(04)       BINARY.
           10 WB-MS-NR                    PIC S9(04)       BINARY.
           10 WB-AZNERF-ANSPR-VOM         PIC  X(10).
           10 WB-AZNERF-ANSPR-SEIT        PIC  X(10).
           10 WB-AZNERF-ANSPR-BIS         PIC  X(10).
           10 WB-AZNERF-EBENE             PIC S9(04)       BINARY.
           10 WB-AZNERF-VERT-FAKTOR       PIC S9(04)       BINARY.
           10 WB-AZNERF-BETR-FAKTOR       PIC S9(09)V9(02) COMP-3.
           10 WB-AZNERF-ANZAHL-MON        PIC S9(04)       BINARY.
           10 WB-AZNERF-BETRAG            PIC S9(09)V9(02) COMP-3.
030822     10 WB-AZNERF-EH-GRENZE         PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZNERF-EH-EIGEN          PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZNERF-EH-EIGEN-MON      PIC S9(10)V9(01) COMP-3.
           10 WB-AZNERF-EQF-EIGEN         PIC S9(03)V9(02) COMP-3.
           10 WB-KNR-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-NERF-GRUND           PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-AZNERF-LAST-UPDATE       PIC  X(26).
030822     10 WB-AZNERF-EG-UFREI          PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZNERF-EG-UPFL           PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZNERF-EG-UFREI-RECH     PIC S9(10)V9(01) COMP-3.
030822     10 WB-AZNERF-EG-UPFL-RECH      PIC S9(10)V9(01) COMP-3.
           10 WB-AZNERF-BETRAG-UPFL       PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  26                    *
      *****************************************************************

