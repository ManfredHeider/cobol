      *****************************************************************
      *    COPY-MEMBER: WVVBA229                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBA229 (FPRB    )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GÄRTTNER                                         *
      *    DATUM:    29.03.2023                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-6478 |29.03.23| NEU                           | Gärttner  *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA229                          *
      *****************************************************************

        05 WB-NI-VIVBA229-TAB.
           10 WB-NI-VIVBA229-ELE   PIC S9(004) BINARY OCCURS  25.
        05 WB-NI-VIVBA229          REDEFINES   WB-NI-VIVBA229-TAB.
           10 WB-NI-FPRB-ST-NR           PIC S9(004) BINARY.
           10 WB-NI-FPRB-STICHTAG        PIC S9(004) BINARY.
           10 WB-NI-FPRB-ANSPR-VOM       PIC S9(004) BINARY.
           10 WB-NI-FPRB-LAST-UPDATE     PIC S9(004) BINARY.
           10 WB-NI-FPRB-MS-NR           PIC S9(004) BINARY.
           10 WB-NI-FPRB-12M-UMS-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRB-12M-EQF         PIC S9(004) BINARY.
           10 WB-NI-FPRB-03M-UMS-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRB-EQF-ABZ-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRB-FAKTOR          PIC S9(004) BINARY.
           10 WB-NI-FPRB-ANZ-MONATE      PIC S9(004) BINARY.
           10 WB-NI-FPRB-12M-UMS-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRB-03M-UMS-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRB-EQF-ABZ-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-ABZ-NEG      PIC S9(004) BINARY.
           10 WB-NI-FPRB-03M-KORR-FREI   PIC S9(004) BINARY.
           10 WB-NI-FPRB-03M-KORR-UPFL   PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-ANT-FREI     PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-ANT-UPFL     PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-GR-UPFL      PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-GR-FREI      PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-GD-UPFL      PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-GD-FREI      PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-ZV-UPFL      PIC S9(004) BINARY.
           10 WB-NI-FPRB-EH-ZV-FREI      PIC S9(004) BINARY.

        05 WB-VIVBA229.
           10 WB-FPRB-ST-NR              PIC S9(009) BINARY.
           10 WB-FPRB-STICHTAG           PIC  X(010).
           10 WB-FPRB-ANSPR-VOM          PIC  X(010).
           10 WB-FPRB-LAST-UPDATE        PIC  X(026).
           10 WB-FPRB-MS-NR              PIC S9(004) BINARY.
           10 WB-FPRB-12M-UMS-FREI       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-12M-EQF            PIC S9(003)V9(002) COMP-3.
           10 WB-FPRB-03M-UMS-FREI       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EQF-ABZ-FREI       PIC S9(009)V9(002) COMP-3.
           10 WB-FPRB-FAKTOR             PIC S9(003)V9(002) COMP-3.
           10 WB-FPRB-ANZ-MONATE         PIC S9(004) BINARY.
           10 WB-FPRB-12M-UMS-UPFL       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-03M-UMS-UPFL       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EQF-ABZ-UPFL       PIC S9(009)V9(002) COMP-3.
           10 WB-FPRB-EH-ABZ-NEG         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-03M-KORR-FREI      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-03M-KORR-UPFL      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EH-ANT-FREI        PIC S9(003)V9(002) COMP-3.
           10 WB-FPRB-EH-ANT-UPFL        PIC S9(003)V9(002) COMP-3.
           10 WB-FPRB-EH-GR-UPFL         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EH-GR-FREI         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EH-GD-UPFL         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EH-GD-FREI         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EH-ZV-UPFL         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRB-EH-ZV-FREI         PIC S9(009)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  25                    *
      *****************************************************************