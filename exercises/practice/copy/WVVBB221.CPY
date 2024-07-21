      *****************************************************************
      *    COPY-MEMBER: WVVBB221                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBB221 (AZNERF  )                       *
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
      * COBOL DEKLARATION FUER VIEW VIVBB221                          *
      *****************************************************************

        05 WB-NI-VIVBB221-TAB.
           10 WB-NI-VIVBB221-ELE   PIC S9(004) BINARY OCCURS  28.
        05 WB-NI-VIVBB221          REDEFINES   WB-NI-VIVBB221-TAB.
           10 WB-NI-AZNERF-PART-VBNR     PIC S9(004) BINARY.
           10 WB-NI-AZNERF-BETR-VBNR     PIC S9(004) BINARY.
           10 WB-NI-DIR-NR               PIC S9(004) BINARY.
           10 WB-NI-MS-NR                PIC S9(004) BINARY.
           10 WB-NI-AZNERF-ANSPR-VOM     PIC S9(004) BINARY.
           10 WB-NI-AZNERF-ANSPR-SEIT    PIC S9(004) BINARY.
           10 WB-NI-AZNERF-ANSPR-BIS     PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EBENE         PIC S9(004) BINARY.
           10 WB-NI-AZNERF-VERT-FAKTOR   PIC S9(004) BINARY.
           10 WB-NI-AZNERF-BETR-FAKTOR   PIC S9(004) BINARY.
           10 WB-NI-AZNERF-ANZAHL-MON    PIC S9(004) BINARY.
           10 WB-NI-AZNERF-BETRAG        PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EH-GRENZE     PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EH-EIGEN      PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EH-EIGEN-MON  PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EQF-EIGEN     PIC S9(004) BINARY.
           10 WB-NI-KNR-NERF-GRUND       PIC S9(004) BINARY.
           10 WB-NI-KTY-NERF-GRUND       PIC S9(004) BINARY.
           10 WB-NI-KTY-WAEHRUNG         PIC S9(004) BINARY.
           10 WB-NI-KNR-WAEHRUNG         PIC S9(004) BINARY.
           10 WB-NI-AZNERF-LAST-UPDATE   PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EG-UFREI      PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EG-UPFL       PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EG-UFREI-RECH PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EG-UPFL-RECH  PIC S9(004) BINARY.
           10 WB-NI-AZNERF-BETRAG-UPFL   PIC S9(004) BINARY.
           10 WB-NI-AZNERF-600-EH-ERF    PIC S9(004) BINARY.
           10 WB-NI-AZNERF-EH-NEU-EIGEN  PIC S9(004) BINARY.

        05 WB-VIVBB221.
           10 WB-AZNERF-PART-VBNR        PIC S9(009) BINARY.
           10 WB-AZNERF-BETR-VBNR        PIC S9(009) BINARY.
           10 WB-DIR-NR                  PIC S9(004) BINARY.
           10 WB-MS-NR                   PIC S9(004) BINARY.
           10 WB-AZNERF-ANSPR-VOM        PIC  X(010).
           10 WB-AZNERF-ANSPR-SEIT       PIC  X(010).
           10 WB-AZNERF-ANSPR-BIS        PIC  X(010).
           10 WB-AZNERF-EBENE            PIC S9(004) BINARY.
           10 WB-AZNERF-VERT-FAKTOR      PIC S9(004) BINARY.
           10 WB-AZNERF-BETR-FAKTOR      PIC S9(009)V9(002) COMP-3.
           10 WB-AZNERF-ANZAHL-MON       PIC S9(004) BINARY.
           10 WB-AZNERF-BETRAG           PIC S9(009)V9(002) COMP-3.
           10 WB-AZNERF-EH-GRENZE        PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-EH-EIGEN         PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-EH-EIGEN-MON     PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-EQF-EIGEN        PIC S9(003)V9(002) COMP-3.
           10 WB-KNR-NERF-GRUND          PIC S9(004) BINARY.
           10 WB-KTY-NERF-GRUND          PIC S9(004) BINARY.
           10 WB-KTY-WAEHRUNG            PIC S9(004) BINARY.
           10 WB-KNR-WAEHRUNG            PIC S9(004) BINARY.
           10 WB-AZNERF-LAST-UPDATE      PIC  X(026).
           10 WB-AZNERF-EG-UFREI         PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-EG-UPFL          PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-EG-UFREI-RECH    PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-EG-UPFL-RECH     PIC S9(010)V9(001) COMP-3.
           10 WB-AZNERF-BETRAG-UPFL      PIC S9(009)V9(002) COMP-3.
           10 WB-AZNERF-600-EH-ERF       PIC  X(001).
           10 WB-AZNERF-EH-NEU-EIGEN     PIC S9(010)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  28                    *
      *****************************************************************
