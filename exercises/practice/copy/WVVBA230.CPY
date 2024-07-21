      *****************************************************************
      *    COPY-MEMBER: WVVBA230                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBA230 (FPRC    )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GAERTTNER                                        *
      *    DATUM:    29.03.2023                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VBL-6478 |29.03.23| NEU                           | Gärttner  *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA230                          *
      *****************************************************************

        05 WB-NI-VIVBA230-TAB.
           10 WB-NI-VIVBA230-ELE   PIC S9(004) BINARY OCCURS  25.
        05 WB-NI-VIVBA230          REDEFINES   WB-NI-VIVBA230-TAB.
           10 WB-NI-FPRC-ST-NR           PIC S9(004) BINARY.
           10 WB-NI-FPRC-STICHTAG        PIC S9(004) BINARY.
           10 WB-NI-FPRC-ANSPR-VOM       PIC S9(004) BINARY.
           10 WB-NI-FPRC-LAST-UPDATE     PIC S9(004) BINARY.
           10 WB-NI-FPRC-MS-NR           PIC S9(004) BINARY.
           10 WB-NI-FPRC-12M-UMS-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRC-12M-EQF         PIC S9(004) BINARY.
           10 WB-NI-FPRC-03M-UMS-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRC-EQF-ABZ-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRC-FAKTOR          PIC S9(004) BINARY.
           10 WB-NI-FPRC-ANZ-MONATE      PIC S9(004) BINARY.
           10 WB-NI-FPRC-12M-UMS-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRC-03M-UMS-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRC-EQF-ABZ-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-ABZ-NEG      PIC S9(004) BINARY.
           10 WB-NI-FPRC-03M-KORR-FREI   PIC S9(004) BINARY.
           10 WB-NI-FPRC-03M-KORR-UPFL   PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-ANT-FREI     PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-ANT-UPFL     PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-GR-UPFL      PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-GR-FREI      PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-GD-UPFL      PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-GD-FREI      PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-ZV-UPFL      PIC S9(004) BINARY.
           10 WB-NI-FPRC-EH-ZV-FREI      PIC S9(004) BINARY.

        05 WB-VIVBA230.
           10 WB-FPRC-ST-NR              PIC S9(009) BINARY.
           10 WB-FPRC-STICHTAG           PIC  X(010).
           10 WB-FPRC-ANSPR-VOM          PIC  X(010).
           10 WB-FPRC-LAST-UPDATE        PIC  X(026).
           10 WB-FPRC-MS-NR              PIC S9(004) BINARY.
           10 WB-FPRC-12M-UMS-FREI       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-12M-EQF            PIC S9(003)V9(002) COMP-3.
           10 WB-FPRC-03M-UMS-FREI       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EQF-ABZ-FREI       PIC S9(009)V9(002) COMP-3.
           10 WB-FPRC-FAKTOR             PIC S9(003)V9(002) COMP-3.
           10 WB-FPRC-ANZ-MONATE         PIC S9(004) BINARY.
           10 WB-FPRC-12M-UMS-UPFL       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-03M-UMS-UPFL       PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EQF-ABZ-UPFL       PIC S9(009)V9(002) COMP-3.
           10 WB-FPRC-EH-ABZ-NEG         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-03M-KORR-FREI      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-03M-KORR-UPFL      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EH-ANT-FREI        PIC S9(003)V9(002) COMP-3.
           10 WB-FPRC-EH-ANT-UPFL        PIC S9(003)V9(002) COMP-3.
           10 WB-FPRC-EH-GR-UPFL         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EH-GR-FREI         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EH-GD-UPFL         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EH-GD-FREI         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EH-ZV-UPFL         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRC-EH-ZV-FREI         PIC S9(009)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  25                    *
      *****************************************************************