      *****************************************************************
      *    ERSTELLT: GAERTTNER                                        *
      *    DATUM:    29.03.2023                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VBL-6478 |29.03.23| NEU                           | GAERTTNER *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA229) FPRB                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA229)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA229 TABLE
CCC004*    ( FPRB_ST_NR                  INTEGER NOT NULL
CCC004*    , FPRB_STICHTAG               DATE NOT NULL
CCC004*    , FPRB_ANSPR_VOM              DATE NOT NULL
CCC004*    , FPRB_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , FPRB_MS_NR                  SMALLINT NOT NULL
CCC004*    , FPRB_12M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_12M_EQF                DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_03M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EQF_ABZ_FREI           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRB_FAKTOR                 DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_ANZ_MONATE             SMALLINT NOT NULL
CCC004*    , FPRB_12M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_03M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EQF_ABZ_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRB_EH_ABZ_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_03M_KORR_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_03M_KORR_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_ANT_FREI            DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_EH_ANT_UPFL            DECIMAL(05, 2) NOT NULL
CCC004*    , FPRB_EH_GR_UPFL             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_GR_FREI             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_GD_UPFL             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_GD_FREI             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_ZV_UPFL             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRB_EH_ZV_FREI             DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA229                          *
      *****************************************************************

       01 VIVBA229-GESAMT.
          05 NI-VIVBA229-TAB.
             10 NI-VIVBA229-ELE    PIC S9(004) BINARY OCCURS  25.
          05 NI-VIVBA229           REDEFINES   NI-VIVBA229-TAB.
             10 NI-FPRB-ST-NR            PIC S9(004) BINARY.
             10 NI-FPRB-STICHTAG         PIC S9(004) BINARY.
             10 NI-FPRB-ANSPR-VOM        PIC S9(004) BINARY.
             10 NI-FPRB-LAST-UPDATE      PIC S9(004) BINARY.
             10 NI-FPRB-MS-NR            PIC S9(004) BINARY.
             10 NI-FPRB-12M-UMS-FREI     PIC S9(004) BINARY.
             10 NI-FPRB-12M-EQF          PIC S9(004) BINARY.
             10 NI-FPRB-03M-UMS-FREI     PIC S9(004) BINARY.
             10 NI-FPRB-EQF-ABZ-FREI     PIC S9(004) BINARY.
             10 NI-FPRB-FAKTOR           PIC S9(004) BINARY.
             10 NI-FPRB-ANZ-MONATE       PIC S9(004) BINARY.
             10 NI-FPRB-12M-UMS-UPFL     PIC S9(004) BINARY.
             10 NI-FPRB-03M-UMS-UPFL     PIC S9(004) BINARY.
             10 NI-FPRB-EQF-ABZ-UPFL     PIC S9(004) BINARY.
             10 NI-FPRB-EH-ABZ-NEG       PIC S9(004) BINARY.
             10 NI-FPRB-03M-KORR-FREI    PIC S9(004) BINARY.
             10 NI-FPRB-03M-KORR-UPFL    PIC S9(004) BINARY.
             10 NI-FPRB-EH-ANT-FREI      PIC S9(004) BINARY.
             10 NI-FPRB-EH-ANT-UPFL      PIC S9(004) BINARY.
             10 NI-FPRB-EH-GR-UPFL       PIC S9(004) BINARY.
             10 NI-FPRB-EH-GR-FREI       PIC S9(004) BINARY.
             10 NI-FPRB-EH-GD-UPFL       PIC S9(004) BINARY.
             10 NI-FPRB-EH-GD-FREI       PIC S9(004) BINARY.
             10 NI-FPRB-EH-ZV-UPFL       PIC S9(004) BINARY.
             10 NI-FPRB-EH-ZV-FREI       PIC S9(004) BINARY.

          05 DCL-VIVBA229.
             10 FPRB-ST-NR               PIC S9(009) BINARY.
             10 FPRB-STICHTAG            PIC  X(010).
             10 FPRB-ANSPR-VOM           PIC  X(010).
             10 FPRB-LAST-UPDATE         PIC  X(026).
             10 FPRB-MS-NR               PIC S9(004) BINARY.
             10 FPRB-12M-UMS-FREI        PIC S9(009)V9(001) COMP-3.
             10 FPRB-12M-EQF             PIC S9(003)V9(002) COMP-3.
             10 FPRB-03M-UMS-FREI        PIC S9(009)V9(001) COMP-3.
             10 FPRB-EQF-ABZ-FREI        PIC S9(009)V9(002) COMP-3.
             10 FPRB-FAKTOR              PIC S9(003)V9(002) COMP-3.
             10 FPRB-ANZ-MONATE          PIC S9(004) BINARY.
             10 FPRB-12M-UMS-UPFL        PIC S9(009)V9(001) COMP-3.
             10 FPRB-03M-UMS-UPFL        PIC S9(009)V9(001) COMP-3.
             10 FPRB-EQF-ABZ-UPFL        PIC S9(009)V9(002) COMP-3.
             10 FPRB-EH-ABZ-NEG          PIC S9(009)V9(001) COMP-3.
             10 FPRB-03M-KORR-FREI       PIC S9(009)V9(001) COMP-3.
             10 FPRB-03M-KORR-UPFL       PIC S9(009)V9(001) COMP-3.
             10 FPRB-EH-ANT-FREI         PIC S9(003)V9(002) COMP-3.
             10 FPRB-EH-ANT-UPFL         PIC S9(003)V9(002) COMP-3.
             10 FPRB-EH-GR-UPFL          PIC S9(009)V9(001) COMP-3.
             10 FPRB-EH-GR-FREI          PIC S9(009)V9(001) COMP-3.
             10 FPRB-EH-GD-UPFL          PIC S9(009)V9(001) COMP-3.
             10 FPRB-EH-GD-FREI          PIC S9(009)V9(001) COMP-3.
             10 FPRB-EH-ZV-UPFL          PIC S9(009)V9(001) COMP-3.
             10 FPRB-EH-ZV-FREI          PIC S9(009)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  25                    *
      *****************************************************************