      *****************************************************************
      *    COPY-MEMBER: WVVB0356                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0356 (WTAUS   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THURMANN                                         *
      *    DATUM:    24.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-4870 |24.08.22| NEU                           | THURMANN  *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0356                          *
      *****************************************************************

        05 WB-NI-VIVB0356-TAB.
           10 WB-NI-VIVB0356-ELE   PIC S9(004) BINARY OCCURS   8.
        05 WB-NI-VIVB0356          REDEFINES   WB-NI-VIVB0356-TAB.
           10 WB-NI-MA-ST-NR             PIC S9(004) BINARY.
           10 WB-NI-WTAUS-ANSPR-VOM      PIC S9(004) BINARY.
           10 WB-NI-KNR-ANSPR-GRUND      PIC S9(004) BINARY.
           10 WB-NI-KTY-ANSPR-GRUND      PIC S9(004) BINARY.
           10 WB-NI-WTAUS-LAST-UPDATE    PIC S9(004) BINARY.
           10 WB-NI-WTAUS-ANSPR-BETRAG   PIC S9(004) BINARY.
           10 WB-NI-KTY-ANSPR-STATUS     PIC S9(004) BINARY.
           10 WB-NI-KNR-ANSPR-STATUS     PIC S9(004) BINARY.

        05 WB-VIVB0356.
           10 WB-MA-ST-NR                PIC S9(009) BINARY.
           10 WB-WTAUS-ANSPR-VOM         PIC  X(010).
           10 WB-KNR-ANSPR-GRUND         PIC S9(004) BINARY.
           10 WB-KTY-ANSPR-GRUND         PIC S9(004) BINARY.
           10 WB-WTAUS-LAST-UPDATE       PIC  X(026).
           10 WB-WTAUS-ANSPR-BETRAG      PIC S9(009)V9(002) COMP-3.
           10 WB-KTY-ANSPR-STATUS        PIC S9(004) BINARY.
           10 WB-KNR-ANSPR-STATUS        PIC S9(004) BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************