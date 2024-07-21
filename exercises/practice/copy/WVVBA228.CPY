      *****************************************************************
      *    COPY-MEMBER: WVVBA228                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBA228 (FPRDD   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIM                                             *
      *    DATUM:    28.03.2023                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-6478 !28.03.23! NEU (VLA-5662)                ! HEIM      *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA228                          *
      *****************************************************************

        05 WB-NI-VIVBA228-TAB.
           10 WB-NI-VIVBA228-ELE   PIC S9(004) BINARY OCCURS  37.
        05 WB-NI-VIVBA228          REDEFINES   WB-NI-VIVBA228-TAB.
           10 WB-NI-FPRDD-ST-NR          PIC S9(004) BINARY.
           10 WB-NI-FPRDD-STICHTAG       PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ANSPR-MONAT    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ANSPR-VOM      PIC S9(004) BINARY.
           10 WB-NI-FPRDD-LAST-UPDATE    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-MS-NR          PIC S9(004) BINARY.
           10 WB-NI-FPRDD-12M-UMS-FREI   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-12M-EQF        PIC S9(004) BINARY.
           10 WB-NI-FPRDD-03M-UMS-FREI   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-PRO-ABZUG-FR   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-N-PRO-ABZ-FR   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ZW-BETRAG      PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EQF-ABZUG-FR   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-BETRAG-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-FAKTOR         PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ANZ-MONATE     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-UMS-BED-ERF    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-S-AUSZAHLUNG   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-S-NACH-DIR     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-12M-UMS-UPFL   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-03M-UMS-UPFL   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-PRO-ABZUG-UP   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-N-PRO-ABZ-UP   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ZW-BETRAG-UP   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EQF-ABZUG-UP   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-BETRAG-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ABZUG-NEG      PIC S9(004) BINARY.
           10 WB-NI-FPRDD-03M-KORR-FRE   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-03M-KORR-UPF   PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ANTEIL-FREI    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-ANTEIL-UPFL    PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EH-GR-UPFL     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EH-GR-FREI     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EH-GD-UPFL     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EH-GD-FREI     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EH-ZV-UPFL     PIC S9(004) BINARY.
           10 WB-NI-FPRDD-EH-ZV-FREI     PIC S9(004) BINARY.

        05 WB-VIVBA228.
           10 WB-FPRDD-ST-NR             PIC S9(009) BINARY.
           10 WB-FPRDD-STICHTAG          PIC  X(010).
           10 WB-FPRDD-ANSPR-MONAT       PIC  X(010).
           10 WB-FPRDD-ANSPR-VOM         PIC  X(010).
           10 WB-FPRDD-LAST-UPDATE       PIC  X(026).
           10 WB-FPRDD-MS-NR             PIC S9(004) BINARY.
           10 WB-FPRDD-12M-UMS-FREI      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-12M-EQF           PIC S9(003)V9(002) COMP-3.
           10 WB-FPRDD-03M-UMS-FREI      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-PRO-ABZUG-FR      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-N-PRO-ABZ-FR      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-ZW-BETRAG         PIC S9(009)V9(002) COMP-3.
           10 WB-FPRDD-EQF-ABZUG-FR      PIC S9(009)V9(002) COMP-3.
           10 WB-FPRDD-BETRAG-FREI       PIC S9(009)V9(002) COMP-3.
           10 WB-FPRDD-FAKTOR            PIC S9(003)V9(002) COMP-3.
           10 WB-FPRDD-ANZ-MONATE        PIC S9(004) BINARY.
           10 WB-FPRDD-UMS-BED-ERF       PIC  X(001).
           10 WB-FPRDD-S-AUSZAHLUNG      PIC  X(001).
           10 WB-FPRDD-S-NACH-DIR        PIC  X(001).
           10 WB-FPRDD-12M-UMS-UPFL      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-03M-UMS-UPFL      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-PRO-ABZUG-UP      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-N-PRO-ABZ-UP      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-ZW-BETRAG-UP      PIC S9(009)V9(002) COMP-3.
           10 WB-FPRDD-EQF-ABZUG-UP      PIC S9(009)V9(002) COMP-3.
           10 WB-FPRDD-BETRAG-UPFL       PIC S9(009)V9(002) COMP-3.
           10 WB-FPRDD-ABZUG-NEG         PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-03M-KORR-FRE      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-03M-KORR-UPF      PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-ANTEIL-FREI       PIC S9(003)V9(002) COMP-3.
           10 WB-FPRDD-ANTEIL-UPFL       PIC S9(003)V9(002) COMP-3.
           10 WB-FPRDD-EH-GR-UPFL        PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-EH-GR-FREI        PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-EH-GD-UPFL        PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-EH-GD-FREI        PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-EH-ZV-UPFL        PIC S9(009)V9(001) COMP-3.
           10 WB-FPRDD-EH-ZV-FREI        PIC S9(009)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  37                    *
      *****************************************************************           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
