      *****************************************************************
      *    COPY-MEMBER: WVVBB220                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBB220 (AZERF   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    04.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-5181 !04.08.22! NEU                           ! U. HEIM   *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBB220                          *
      *****************************************************************

        05 WB-NI-VIVBB220-TAB.
           10 WB-NI-VIVBB220-ELE   PIC S9(004) BINARY OCCURS  26.
        05 WB-NI-VIVBB220          REDEFINES   WB-NI-VIVBB220-TAB.
           10 WB-NI-AZERF-PART-VBNR      PIC S9(004) BINARY.
           10 WB-NI-AZERF-BETR-VBNR      PIC S9(004) BINARY.
           10 WB-NI-DIR-NR               PIC S9(004) BINARY.
           10 WB-NI-MS-NR                PIC S9(004) BINARY.
           10 WB-NI-AZERF-ANSPR-VOM      PIC S9(004) BINARY.
           10 WB-NI-AZERF-ANSPR-SEIT     PIC S9(004) BINARY.
           10 WB-NI-AZERF-ANSPR-BIS      PIC S9(004) BINARY.
           10 WB-NI-AZERF-EBENE          PIC S9(004) BINARY.
           10 WB-NI-AZERF-VERT-FAKTOR    PIC S9(004) BINARY.
           10 WB-NI-AZERF-BETR-FAKTOR    PIC S9(004) BINARY.
           10 WB-NI-AZERF-ANZAHL-MON     PIC S9(004) BINARY.
           10 WB-NI-AZERF-BETRAG         PIC S9(004) BINARY.
           10 WB-NI-AZERF-EH-GRENZE      PIC S9(004) BINARY.
           10 WB-NI-AZERF-EH-EIGEN       PIC S9(004) BINARY.
           10 WB-NI-AZERF-EH-EIGEN-MON   PIC S9(004) BINARY.
           10 WB-NI-AZERF-EQF-EIGEN      PIC S9(004) BINARY.
           10 WB-NI-KTY-WAEHRUNG         PIC S9(004) BINARY.
           10 WB-NI-KNR-WAEHRUNG         PIC S9(004) BINARY.
           10 WB-NI-AZERF-LAST-UPDATE    PIC S9(004) BINARY.
           10 WB-NI-AZERF-EG-UFREI       PIC S9(004) BINARY.
           10 WB-NI-AZERF-EG-UPFL        PIC S9(004) BINARY.
           10 WB-NI-AZERF-EG-UFREI-RECH  PIC S9(004) BINARY.
           10 WB-NI-AZERF-EG-UPFL-RECH   PIC S9(004) BINARY.
           10 WB-NI-AZERF-BETRAG-UPFL    PIC S9(004) BINARY.
           10 WB-NI-AZERF-600-EH-ERF     PIC S9(004) BINARY.
           10 WB-NI-AZERF-EH-NEU-EIGEN   PIC S9(004) BINARY.

        05 WB-VIVBB220.
           10 WB-AZERF-PART-VBNR         PIC S9(009) BINARY.
           10 WB-AZERF-BETR-VBNR         PIC S9(009) BINARY.
           10 WB-DIR-NR                  PIC S9(004) BINARY.
           10 WB-MS-NR                   PIC S9(004) BINARY.
           10 WB-AZERF-ANSPR-VOM         PIC  X(010).
           10 WB-AZERF-ANSPR-SEIT        PIC  X(010).
           10 WB-AZERF-ANSPR-BIS         PIC  X(010).
           10 WB-AZERF-EBENE             PIC S9(004) BINARY.
           10 WB-AZERF-VERT-FAKTOR       PIC S9(004) BINARY.
           10 WB-AZERF-BETR-FAKTOR       PIC S9(009)V9(002) COMP-3.
           10 WB-AZERF-ANZAHL-MON        PIC S9(004) BINARY.
           10 WB-AZERF-BETRAG            PIC S9(009)V9(002) COMP-3.
           10 WB-AZERF-EH-GRENZE         PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-EH-EIGEN          PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-EH-EIGEN-MON      PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-EQF-EIGEN         PIC S9(003)V9(002) COMP-3.
           10 WB-KTY-WAEHRUNG            PIC S9(004) BINARY.
           10 WB-KNR-WAEHRUNG            PIC S9(004) BINARY.
           10 WB-AZERF-LAST-UPDATE       PIC  X(026).
           10 WB-AZERF-EG-UFREI          PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-EG-UPFL           PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-EG-UFREI-RECH     PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-EG-UPFL-RECH      PIC S9(010)V9(001) COMP-3.
           10 WB-AZERF-BETRAG-UPFL       PIC S9(009)V9(002) COMP-3.
           10 WB-AZERF-600-EH-ERF        PIC  X(001).
           10 WB-AZERF-EH-NEU-EIGEN      PIC S9(010)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  26                    *
      *****************************************************************
