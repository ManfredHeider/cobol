      *****************************************************************
      *    ERSTELLT: GAERTTNER                                        *
      *    DATUM:    29.03.2023                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-6467 |29.03.23| NEU                           | GAERTTNER *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA228) FPRDD                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA228)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA228 TABLE
CCC004*    ( FPRDD_ST_NR                 INTEGER NOT NULL
CCC004*    , FPRDD_STICHTAG              DATE NOT NULL
CCC004*    , FPRDD_ANSPR_MONAT           DATE NOT NULL
CCC004*    , FPRDD_ANSPR_VOM             DATE NOT NULL
CCC004*    , FPRDD_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , FPRDD_MS_NR                 SMALLINT NOT NULL
CCC004*    , FPRDD_12M_UMS_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_12M_EQF               DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_03M_UMS_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_PRO_ABZUG_FR          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_N_PRO_ABZ_FR          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_ZW_BETRAG             DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_EQF_ABZUG_FR          DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_BETRAG_FREI           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_FAKTOR                DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_ANZ_MONATE            SMALLINT NOT NULL
CCC004*    , FPRDD_UMS_BED_ERF           CHAR(001) NOT NULL
CCC004*    , FPRDD_S_AUSZAHLUNG          CHAR(001) NOT NULL
CCC004*    , FPRDD_S_NACH_DIR            CHAR(001) NOT NULL
CCC004*    , FPRDD_12M_UMS_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_03M_UMS_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_PRO_ABZUG_UP          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_N_PRO_ABZ_UP          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_ZW_BETRAG_UP          DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_EQF_ABZUG_UP          DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_BETRAG_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRDD_ABZUG_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_03M_KORR_FRE          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_03M_KORR_UPF          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_ANTEIL_FREI           DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_ANTEIL_UPFL           DECIMAL(05, 2) NOT NULL
CCC004*    , FPRDD_EH_GR_UPFL            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_EH_GR_FREI            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_EH_GD_UPFL            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_EH_GD_FREI            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_EH_ZV_UPFL            DECIMAL(10, 1) NOT NULL
CCC004*    , FPRDD_EH_ZV_FREI            DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA228                          *
      *****************************************************************

       01 VIVBA228-GESAMT.
          05 NI-VIVBA228-TAB.
             10 NI-VIVBA228-ELE    PIC S9(004) BINARY OCCURS  37.
          05 NI-VIVBA228           REDEFINES   NI-VIVBA228-TAB.
             10 NI-FPRDD-ST-NR           PIC S9(004) BINARY.
             10 NI-FPRDD-STICHTAG        PIC S9(004) BINARY.
             10 NI-FPRDD-ANSPR-MONAT     PIC S9(004) BINARY.
             10 NI-FPRDD-ANSPR-VOM       PIC S9(004) BINARY.
             10 NI-FPRDD-LAST-UPDATE     PIC S9(004) BINARY.
             10 NI-FPRDD-MS-NR           PIC S9(004) BINARY.
             10 NI-FPRDD-12M-UMS-FREI    PIC S9(004) BINARY.
             10 NI-FPRDD-12M-EQF         PIC S9(004) BINARY.
             10 NI-FPRDD-03M-UMS-FREI    PIC S9(004) BINARY.
             10 NI-FPRDD-PRO-ABZUG-FR    PIC S9(004) BINARY.
             10 NI-FPRDD-N-PRO-ABZ-FR    PIC S9(004) BINARY.
             10 NI-FPRDD-ZW-BETRAG       PIC S9(004) BINARY.
             10 NI-FPRDD-EQF-ABZUG-FR    PIC S9(004) BINARY.
             10 NI-FPRDD-BETRAG-FREI     PIC S9(004) BINARY.
             10 NI-FPRDD-FAKTOR          PIC S9(004) BINARY.
             10 NI-FPRDD-ANZ-MONATE      PIC S9(004) BINARY.
             10 NI-FPRDD-UMS-BED-ERF     PIC S9(004) BINARY.
             10 NI-FPRDD-S-AUSZAHLUNG    PIC S9(004) BINARY.
             10 NI-FPRDD-S-NACH-DIR      PIC S9(004) BINARY.
             10 NI-FPRDD-12M-UMS-UPFL    PIC S9(004) BINARY.
             10 NI-FPRDD-03M-UMS-UPFL    PIC S9(004) BINARY.
             10 NI-FPRDD-PRO-ABZUG-UP    PIC S9(004) BINARY.
             10 NI-FPRDD-N-PRO-ABZ-UP    PIC S9(004) BINARY.
             10 NI-FPRDD-ZW-BETRAG-UP    PIC S9(004) BINARY.
             10 NI-FPRDD-EQF-ABZUG-UP    PIC S9(004) BINARY.
             10 NI-FPRDD-BETRAG-UPFL     PIC S9(004) BINARY.
             10 NI-FPRDD-ABZUG-NEG       PIC S9(004) BINARY.
             10 NI-FPRDD-03M-KORR-FRE    PIC S9(004) BINARY.
             10 NI-FPRDD-03M-KORR-UPF    PIC S9(004) BINARY.
             10 NI-FPRDD-ANTEIL-FREI     PIC S9(004) BINARY.
             10 NI-FPRDD-ANTEIL-UPFL     PIC S9(004) BINARY.
             10 NI-FPRDD-EH-GR-UPFL      PIC S9(004) BINARY.
             10 NI-FPRDD-EH-GR-FREI      PIC S9(004) BINARY.
             10 NI-FPRDD-EH-GD-UPFL      PIC S9(004) BINARY.
             10 NI-FPRDD-EH-GD-FREI      PIC S9(004) BINARY.
             10 NI-FPRDD-EH-ZV-UPFL      PIC S9(004) BINARY.
             10 NI-FPRDD-EH-ZV-FREI      PIC S9(004) BINARY.

          05 DCL-VIVBA228.
             10 FPRDD-ST-NR              PIC S9(009) BINARY.
             10 FPRDD-STICHTAG           PIC  X(010).
             10 FPRDD-ANSPR-MONAT        PIC  X(010).
             10 FPRDD-ANSPR-VOM          PIC  X(010).
             10 FPRDD-LAST-UPDATE        PIC  X(026).
             10 FPRDD-MS-NR              PIC S9(004) BINARY.
             10 FPRDD-12M-UMS-FREI       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-12M-EQF            PIC S9(003)V9(002) COMP-3.
             10 FPRDD-03M-UMS-FREI       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-PRO-ABZUG-FR       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-N-PRO-ABZ-FR       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-ZW-BETRAG          PIC S9(009)V9(002) COMP-3.
             10 FPRDD-EQF-ABZUG-FR       PIC S9(009)V9(002) COMP-3.
             10 FPRDD-BETRAG-FREI        PIC S9(009)V9(002) COMP-3.
             10 FPRDD-FAKTOR             PIC S9(003)V9(002) COMP-3.
             10 FPRDD-ANZ-MONATE         PIC S9(004) BINARY.
             10 FPRDD-UMS-BED-ERF        PIC  X(001).
             10 FPRDD-S-AUSZAHLUNG       PIC  X(001).
             10 FPRDD-S-NACH-DIR         PIC  X(001).
             10 FPRDD-12M-UMS-UPFL       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-03M-UMS-UPFL       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-PRO-ABZUG-UP       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-N-PRO-ABZ-UP       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-ZW-BETRAG-UP       PIC S9(009)V9(002) COMP-3.
             10 FPRDD-EQF-ABZUG-UP       PIC S9(009)V9(002) COMP-3.
             10 FPRDD-BETRAG-UPFL        PIC S9(009)V9(002) COMP-3.
             10 FPRDD-ABZUG-NEG          PIC S9(009)V9(001) COMP-3.
             10 FPRDD-03M-KORR-FRE       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-03M-KORR-UPF       PIC S9(009)V9(001) COMP-3.
             10 FPRDD-ANTEIL-FREI        PIC S9(003)V9(002) COMP-3.
             10 FPRDD-ANTEIL-UPFL        PIC S9(003)V9(002) COMP-3.
             10 FPRDD-EH-GR-UPFL         PIC S9(009)V9(001) COMP-3.
             10 FPRDD-EH-GR-FREI         PIC S9(009)V9(001) COMP-3.
             10 FPRDD-EH-GD-UPFL         PIC S9(009)V9(001) COMP-3.
             10 FPRDD-EH-GD-FREI         PIC S9(009)V9(001) COMP-3.
             10 FPRDD-EH-ZV-UPFL         PIC S9(009)V9(001) COMP-3.
             10 FPRDD-EH-ZV-FREI         PIC S9(009)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  37                    *
      *****************************************************************